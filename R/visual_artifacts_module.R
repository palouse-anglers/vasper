# Visual Artifacts Module ----

#' Visual artifacts page UI
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @export
visual_artifacts_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "visual-artifacts-page",
    div(
      class = "data-page-toolbar",
      h5(class = "mb-0", "Visual artifacts"),
      actionButton(
        ns("open_manage_views"),
        label = "Manage views",
        icon = icon("table-columns"),
        class = "btn btn-sm btn-outline-primary"
      )
    ),
    p(
      class = "text-muted small mb-3",
      paste(
        "Browse generated visual artifacts.",
        "Click the chat icon to refine a chart."
      )
    ),
    uiOutput(ns("artifacts_list_ui"))
  )
}

artifact_input_key <- function(artifact_id) {
  base <- gsub("[^A-Za-z0-9_]+", "_", artifact_id)
  base <- gsub("_+", "_", base)
  base <- gsub("^_|_$", "", base)

  if (!nzchar(base)) {
    base <- "artifact"
  }

  max_key_chars <- 80L
  if (nchar(base) > max_key_chars) {
    base <- substr(base, 1, max_key_chars)
    base <- gsub("_+$", "", base)
  }

  base
}

# -- Ephemeral artifact chat system prompt builder ----------------------------

build_artifact_chat_system_prompt <- function(artifact) {
  spec <- artifact$spec %||% list()
  desc <- artifact$description %||% spec$description %||% "(no description)"
  schema_name <- spec$schema_name %||% "(freeform code)"
  code <- spec$plot_code %||% "(not available)"

  paste0(
    "You are a visualization refinement assistant in a subchat.\n",
    "You are editing exactly ONE chart artifact.\n",
    "Keep responses concise for mobile:\n",
    "- default: <= 60 words or <= 4 short bullets\n",
    "- no long explanations unless asked\n",
    "- focus only on the current artifact\n\n",
    "## Current artifact\n",
    "- artifact_id: ",
    artifact$artifact_id,
    "\n",
    "- type: ",
    artifact$artifact_type %||% "plot",
    "\n",
    "- schema: ",
    schema_name,
    "\n",
    "- table: ",
    artifact$table_name %||% spec$table_name %||% "(unknown)",
    "\n",
    "- description: ",
    desc,
    "\n",
    "- title: ",
    artifact$title %||% "(none)",
    "\n",
    "- subtitle: ",
    artifact$subtitle %||% "(none)",
    "\n\n",
    "## Current code\n```r\n",
    code,
    "\n```\n\n",
    "## Instructions\n",
    "- Use list_plot_schemas to browse summaries, then read_plot_schemas for selected templates.\n",
    "- Then use create_plot_from_schema or create_plot_code.\n",
    "- Use rerender_visual_artifact to tweak the existing artifact with overrides.\n",
    "- Use query_tables if you need data exploration. Persisted query results must use NEW table names.\n",
    "- Use get_table_profile when you need deep diagnostics for one table.\n",
    "- Before changing mapped columns, use profile output to avoid all-missing/high-missing fields.\n",
    "- The artifact_name for any new plot should be '",
    artifact$artifact_id,
    "' to replace the current one.\n",
    "- If the user's request is beyond your tools, compose a message and tell the user:\n",
    "  'This requires the main chat. I've sent a request — switch there to continue.'\n",
    "  Then use the escalate_to_main_chat tool.\n"
  )
}

#' Visual artifacts page server
#'
#' @param id Module id
#' @param con DBI connection
#' @param refresh_nonce_r Reactive expression for external refresh triggers
#' @param on_refresh_cb Optional callback after rerender
#'
#' @return List API
#' @export
visual_artifacts_server <- function(
  id,
  con,
  refresh_nonce_r = reactive(NULL),
  on_refresh_cb = NULL
) {
  moduleServer(id, function(input, output, session) {
    collapsed_state <- reactiveVal(setNames(logical(0), character(0)))
    registered_action_ids <- reactiveVal(character())
    artifact_chat_observers <- reactiveVal(list())
    artifact_chat_sessions <- reactiveVal(list())
    hidden_artifact_ids <- reactiveVal(character())

    artifacts_r <- reactive({
      refresh_nonce_r()
      get_visual_artifact_metadata(con, include_inactive = FALSE)$artifacts
    })

    visible_artifacts_r <- reactive({
      artifacts <- artifacts_r()
      hidden <- hidden_artifact_ids()

      if (length(hidden) == 0) {
        return(artifacts)
      }

      artifacts[
        !vapply(
          artifacts,
          function(a) (a$artifact_id %||% "") %in% hidden,
          logical(1)
        )
      ]
    })

    observe({
      artifacts <- artifacts_r()
      artifact_ids <- vapply(
        artifacts,
        function(a) a$artifact_id %||% "",
        character(1)
      )

      hidden <- isolate(hidden_artifact_ids())
      hidden <- intersect(hidden, artifact_ids)
      hidden_artifact_ids(hidden)

      state <- isolate(collapsed_state())
      state <- state[names(state) %in% artifact_ids]

      new_ids <- setdiff(artifact_ids, names(state))
      if (length(new_ids) > 0) {
        new_state <- rep(TRUE, length(new_ids))
        names(new_state) <- new_ids
        state <- c(state, new_state)
      }

      collapsed_state(state)
    })

    format_artifact_choice <- function(artifact) {
      label_title <- artifact$artifact_label %||% artifact$artifact_id %||% ""
      label_title <- sub("^plot\\s+", "", label_title, ignore.case = TRUE)

      dim_label <- paste0(
        artifact$artifact_type %||% "unknown",
        " - table: ",
        artifact$table_name %||% ""
      )

      tags$div(
        class = "data-table-choice",
        tags$div(
          class = "data-table-choice-header",
          tags$span(
            class = "data-table-choice-name font-monospace",
            artifact$artifact_id %||% ""
          )
        ),
        tags$div(
          class = "small text-muted data-table-choice-meta",
          paste0(
            label_title,
            " - ",
            dim_label
          )
        )
      )
    }

    observeEvent(input$open_manage_views, {
      artifacts <- artifacts_r()

      if (length(artifacts) == 0) {
        showNotification("No visual artifacts available.", type = "warning")
        return(invisible(NULL))
      }

      artifact_ids <- vapply(
        artifacts,
        function(a) a$artifact_id %||% "",
        character(1)
      )
      hidden <- isolate(hidden_artifact_ids())
      selected <- setdiff(artifact_ids, hidden)

      showModal(
        modalDialog(
          title = "Manage visual artifact views",
          tags$div(class = "small text-muted mb-2", "Select artifact(s)"),
          checkboxGroupInput(
            session$ns("manage_artifact_selection"),
            label = NULL,
            choiceNames = lapply(artifacts, format_artifact_choice),
            choiceValues = artifact_ids,
            selected = selected,
            width = "100%"
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirm_manage_views"),
              label = "Apply",
              class = "btn btn-primary"
            )
          ),
          size = "m",
          class = "data-views-modal"
        )
      )

      invisible(NULL)
    })

    observeEvent(input$confirm_manage_views, {
      artifacts <- artifacts_r()
      artifact_ids <- vapply(
        artifacts,
        function(a) a$artifact_id %||% "",
        character(1)
      )

      selected <- input$manage_artifact_selection %||% character()
      selected <- unique(as.character(selected))
      selected <- intersect(selected, artifact_ids)

      hidden_artifact_ids(setdiff(artifact_ids, selected))
      removeModal()

      invisible(NULL)
    })

    get_artifact_by_id <- function(artifact_id) {
      artifacts <- isolate(artifacts_r())

      idx <- which(vapply(
        artifacts,
        function(a) identical(a$artifact_id, artifact_id),
        logical(1)
      ))

      if (length(idx) == 0) {
        return(NULL)
      }

      artifacts[[idx[[1]]]]
    }

    # -- Build restricted tools for ephemeral chat ----------------------------

    build_artifact_tools <- function(artifact_id) {
      tool_list_schemas <- tool(
        function() run_list_plot_schemas(),
        name = "list_plot_schemas",
        description = "List available plot schemas (summary only)."
      )

      # read_plot_schemas — no DB, pure catalog
      tool_read_schemas <- tool(
        function(schema_names) {
          run_read_plot_schemas(schema_names = schema_names)
        },
        name = "read_plot_schemas",
        description = "Return full template payload for selected schema names.",
        arguments = list(
          schema_names = type_array(
            type_string(),
            "Schema names selected from list_plot_schemas."
          )
        )
      )

      # create_plot_from_schema — wired to the session DB
      tool_schema_plot <- tool(
        function(
          schema_name,
          table_name,
          column_map,
          description,
          title = NULL,
          subtitle = NULL,
          artifact_name,
          artifact_label = NULL,
          width = 9,
          height = 5,
          dpi = 180
        ) {
          result <- run_create_plot_from_schema(
            con = con,
            schema_name = schema_name,
            table_name = table_name,
            column_map = column_map,
            description = description,
            title = title,
            subtitle = subtitle,
            artifact_name = artifact_name,
            artifact_label = artifact_label,
            width = width,
            height = height,
            dpi = dpi,
            add_data_view = FALSE
          )
          if (is.function(on_refresh_cb)) {
            on_refresh_cb()
          }
          showNotification("Plot created.", type = "message")
          removeModal()
          result
        },
        name = "create_plot_from_schema",
        description = "Create a plot from a named schema with column mappings. Requires artifact_name.",
        arguments = list(
          schema_name = type_string("Schema name."),
          table_name = type_string("DuckDB table name."),
          column_map = plot_column_map_type("Column mappings."),
          description = type_string("What the plot shows."),
          title = type_string("Chart title.", required = FALSE),
          subtitle = type_string("Chart subtitle.", required = FALSE),
          artifact_name = type_string("Artifact id slug.", required = TRUE),
          artifact_label = type_string("Artifact label.", required = FALSE),
          width = type_number("Width inches.", required = FALSE),
          height = type_number("Height inches.", required = FALSE),
          dpi = type_number("PNG DPI.", required = FALSE)
        )
      )

      # create_plot_code
      tool_code_plot <- tool(
        function(
          plot_code,
          table_names,
          description,
          inspiration_schemas,
          artifact_name,
          artifact_label = NULL,
          title = NULL,
          subtitle = NULL,
          width = 9,
          height = 5,
          dpi = 180,
          limit_rows = 50000
        ) {
          result <- run_create_plot_code(
            con = con,
            plot_code = plot_code,
            table_names = table_names,
            description = description,
            inspiration_schemas = inspiration_schemas,
            artifact_name = artifact_name,
            artifact_label = artifact_label,
            title = title,
            subtitle = subtitle,
            width = width,
            height = height,
            dpi = dpi,
            limit_rows = as.integer(limit_rows),
            add_data_view = FALSE
          )
          if (is.function(on_refresh_cb)) {
            on_refresh_cb()
          }
          showNotification("Plot created.", type = "message")
          removeModal()
          result
        },
        name = "create_plot_code",
        description = "Create a plot from R code. Requires inspiration_schemas and artifact_name.",
        arguments = list(
          plot_code = type_string("R code evaluating to ggplot."),
          table_names = type_array(type_string(), "DuckDB table names."),
          description = type_string("What the plot shows."),
          inspiration_schemas = type_array(
            type_string(),
            "Schema names for inspiration."
          ),
          artifact_name = type_string("Artifact id.", required = TRUE),
          artifact_label = type_string("Artifact label.", required = FALSE),
          title = type_string("Title.", required = FALSE),
          subtitle = type_string("Subtitle.", required = FALSE),
          width = type_number("Width.", required = FALSE),
          height = type_number("Height.", required = FALSE),
          dpi = type_number("DPI.", required = FALSE),
          limit_rows = type_number("Row limit.", required = FALSE)
        )
      )

      # rerender_visual_artifact
      tool_rerender <- tool(
        function(artifact_id, overrides = list()) {
          result <- rerender_visual_artifact(
            con = con,
            artifact_id = artifact_id,
            overrides = overrides,
            add_data_view = FALSE
          )
          if (is.function(on_refresh_cb)) {
            on_refresh_cb()
          }
          result
        },
        name = "rerender_visual_artifact",
        description = "Re-render an existing artifact with spec overrides.",
        arguments = list(
          artifact_id = type_string("Artifact id to rerender."),
          overrides = type_object(
            .description = "Spec overrides.",
            .required = FALSE,
            .additional_properties = TRUE
          )
        )
      )

      # query_tables
      tool_query <- tool(
        function(
          sql,
          mode = "free",
          input_tables = NULL,
          output_table_names = NULL,
          output_table_labels = NULL,
          persist = FALSE,
          add_data_view = TRUE
        ) {
          run_query_tables(
            con = con,
            sql = sql,
            mode = mode,
            input_tables = input_tables,
            output_table_names = output_table_names,
            output_table_labels = output_table_labels,
            persist = isTRUE(persist),
            add_data_view = isTRUE(add_data_view)
          )
        },
        name = "query_tables",
        description = paste(
          "Run SQL queries against DuckDB tables.",
          paste(
            "Set persist=TRUE with output_table_names/output_table_labels",
            "to save results with NEW table names."
          )
        ),
        arguments = list(
          sql = type_string("SQL query."),
          mode = type_enum(
            "Query mode.",
            values = c("free", "vectorized"),
            required = FALSE
          ),
          input_tables = type_array(
            type_string(),
            "Tables for vectorized mode.",
            required = FALSE
          ),
          output_table_names = type_array(
            type_string(),
            "Output table name(s). Required when persist is TRUE."
          ),
          output_table_labels = type_array(
            type_string(),
            "Output table label(s). Required when persist is TRUE."
          ),
          persist = type_boolean(
            "Whether to persist query results as table(s).",
            required = FALSE
          ),
          add_data_view = type_boolean(
            "Whether persisted result tables should be added to Data views.",
            required = FALSE
          )
        )
      )

      tool_profile <- tool(
        function(table_name, sample_values_n = 3, max_sample_chars = 120) {
          get_table_profile(
            con = con,
            table_name = table_name,
            sample_values_n = as.integer(sample_values_n),
            max_sample_chars = as.integer(max_sample_chars)
          )
        },
        name = "get_table_profile",
        description = paste(
          "Profile one table with per-column missingness,",
          "distinct counts, and unique sample values."
        ),
        arguments = list(
          table_name = type_string("Table name to profile."),
          sample_values_n = type_number(
            "Distinct sample values per column (default 3, max 20).",
            required = FALSE
          ),
          max_sample_chars = type_number(
            "Max characters per sampled value (default 120).",
            required = FALSE
          )
        )
      )

      # escalate_to_main_chat
      tool_escalate <- tool(
        function(message) {
          session$sendCustomMessage(
            "escalate_to_main_chat",
            list(message = message)
          )
          list(ok = TRUE, message = "Escalated to main chat.")
        },
        name = "escalate_to_main_chat",
        description = paste(
          "Send a message to the main chat when this request exceeds",
          "your available tools. The modal will close automatically."
        ),
        arguments = list(
          message = type_string("The message to send to the main chat.")
        )
      )

      list(
        tool_list_schemas,
        tool_read_schemas,
        tool_schema_plot,
        tool_code_plot,
        tool_rerender,
        tool_query,
        tool_profile,
        tool_escalate
      )
    }

    # -- Open ephemeral chat modal --------------------------------------------

    show_artifact_chat <- function(artifact) {
      req(!is.null(artifact))

      sessions <- isolate(artifact_chat_sessions())
      session_key <- artifact$artifact_id
      chat_session <- sessions[[session_key]]

      if (is.null(chat_session)) {
        sys_prompt <- build_artifact_chat_system_prompt(artifact)
        chat_client <- create_chat_client(sys_prompt)
        tools <- build_artifact_tools(artifact$artifact_id)
        registration_ok <- tryCatch(
          {
            chat_client$register_tools(tools)
            TRUE
          },
          error = function(e) {
            showNotification(
              paste0(
                "Unable to initialize artifact chat tools: ",
                conditionMessage(e)
              ),
              type = "error"
            )
            FALSE
          }
        )
        req(registration_ok)

        chat_session <- list(
          chat_id = paste0(
            "artifact_chat_",
            artifact_input_key(artifact$artifact_id)
          ),
          chat_client = chat_client
        )
        sessions[[session_key]] <- chat_session
        artifact_chat_sessions(sessions)
      }

      local_chat_id <- chat_session$chat_id
      chat_client <- chat_session$chat_client
      chat_ui_id <- session$ns(local_chat_id)

      showModal(
        modalDialog(
          title = paste0(
            "Chat: ",
            artifact$artifact_label %||% artifact$artifact_id
          ),
          size = "l",
          easyClose = TRUE,
          tags$div(
            style = "min-height: 350px; max-height: 60vh; overflow-y: auto;",
            chat_ui(chat_ui_id, messages = list(), fill = FALSE)
          ),
          footer = modalButton("Close")
        )
      )

      session$onFlushed(
        function() {
          render_chat_turns(chat_client$get_turns(), chat_id = local_chat_id)
        },
        once = TRUE
      )

      prev_observers <- isolate(artifact_chat_observers())
      artifact_chat_observers(register_subchat_streaming(
        session = session,
        input = input,
        user_input_id = paste0(local_chat_id, "_user_input"),
        chat_id = local_chat_id,
        chat_client = chat_client,
        existing_observers = prev_observers,
        stream_error_prefix = "Stream error: "
      ))
    }

    # -- Action observers & rendering -----------------------------------------

    observe({
      artifacts <- visible_artifacts_r()
      current <- isolate(registered_action_ids())

      for (artifact in artifacts) {
        artifact_id <- artifact$artifact_id %||% ""
        if (!nzchar(artifact_id)) {
          next
        }

        key <- artifact_input_key(artifact_id)
        toggle_id <- paste0("toggle_", key)
        chat_id <- paste0("chat_", key)
        remove_id <- paste0("remove_", key)
        output_id <- paste0("preview_", key)

        if (!toggle_id %in% current) {
          local({
            local_artifact_id <- artifact_id
            local_toggle_id <- toggle_id

            observeEvent(
              input[[local_toggle_id]],
              {
                state <- isolate(collapsed_state())
                current_value <- state[[local_artifact_id]]
                if (is.null(current_value)) {
                  current_value <- TRUE
                }
                state[[local_artifact_id]] <- !isTRUE(current_value)
                collapsed_state(state)
              },
              ignoreInit = TRUE
            )
          })

          current <- c(current, toggle_id)
        }

        if (!chat_id %in% current) {
          local({
            local_artifact_id <- artifact_id
            local_chat_id <- chat_id

            observeEvent(
              input[[local_chat_id]],
              {
                artifact_now <- get_artifact_by_id(local_artifact_id)
                req(!is.null(artifact_now))
                show_artifact_chat(artifact_now)
              },
              ignoreInit = TRUE
            )
          })

          current <- c(current, chat_id)
        }

        if (!remove_id %in% current) {
          local({
            local_artifact_id <- artifact_id
            local_remove_id <- remove_id

            observeEvent(
              input[[local_remove_id]],
              {
                hidden <- isolate(hidden_artifact_ids())
                hidden_artifact_ids(unique(c(hidden, local_artifact_id)))
              },
              ignoreInit = TRUE
            )
          })

          current <- c(current, remove_id)
        }

        local({
          local_artifact_id <- artifact_id
          local_output_id <- output_id

          output[[local_output_id]] <- renderImage(
            {
              artifacts_now <- artifacts_r()
              idx <- which(vapply(
                artifacts_now,
                function(a) identical(a$artifact_id, local_artifact_id),
                logical(1)
              ))
              req(length(idx) > 0)

              artifact_now <- artifacts_now[[idx[[1]]]]
              path <- artifact_now$png_path %||% ""
              req(nzchar(path), file.exists(path))

              list(
                src = path,
                alt = artifact_now$artifact_label %||% artifact_now$artifact_id,
                contentType = "image/png"
              )
            },
            deleteFile = FALSE
          )
        })
      }

      registered_action_ids(current)
    })

    output$artifacts_list_ui <- renderUI({
      artifacts <- visible_artifacts_r()

      if (length(artifacts) == 0) {
        return(
          div(
            class = "text-muted small mb-2",
            "No visualizations yet. Request one in the Chat."
          )
        )
      }

      state <- collapsed_state()

      tagList(lapply(artifacts, function(artifact) {
        artifact_id <- artifact$artifact_id %||% ""
        req(nzchar(artifact_id))

        key <- artifact_input_key(artifact_id)
        toggle_id <- paste0("toggle_", key)
        chat_id <- paste0("chat_", key)
        remove_id <- paste0("remove_", key)
        output_id <- paste0("preview_", key)
        collapsed <- isTRUE(state[[artifact_id]])

        summary_title <- artifact$artifact_label %||% artifact_id
        summary_title <- sub("^plot\\s+", "", summary_title, ignore.case = TRUE)
        if (!nzchar(trimws(summary_title))) {
          summary_title <- artifact_id
        }
        summary_subtitle <- paste0(
          artifact$artifact_type %||% "unknown",
          " \u2022 table: ",
          artifact$table_name %||% ""
        )

        div(
          class = paste(
            "visual-artifact-item card",
            if (collapsed) "visual-artifact-collapsed"
          ),
          div(
            class = "card-header visual-artifact-header-row",
            div(
              class = "visual-artifact-summary",
              span(class = "visual-artifact-title", summary_title),
              div(class = "small text-muted", summary_subtitle)
            ),
            div(
              class = "visual-artifact-controls",
              actionButton(
                session$ns(chat_id),
                label = NULL,
                icon = icon("comments"),
                class = "btn btn-sm btn-outline-primary",
                title = "Refine with AI chat"
              ),
              actionButton(
                session$ns(toggle_id),
                label = NULL,
                icon = icon(if (collapsed) "chevron-down" else "chevron-up"),
                class = "btn btn-sm btn-outline-secondary",
                title = if (collapsed) "Expand" else "Collapse"
              ),
              actionButton(
                session$ns(remove_id),
                label = NULL,
                icon = icon("trash"),
                class = "btn btn-sm btn-outline-danger",
                title = "Remove from current view"
              )
            )
          ),
          if (!collapsed) {
            div(
              class = "card-body",
              div(
                class = "visual-artifacts-preview-image-wrap",
                imageOutput(session$ns(output_id), height = "420px")
              )
            )
          }
        )
      }))
    })

    # Note: Escalation to the main chat is handled entirely by the JS handler
    # in navigation.js. It navigates to chat, closes the modal, and sets the
    # global Shiny input for the main chat to pick up.

    list(
      get_artifacts = function() artifacts_r()
    )
  })
}
