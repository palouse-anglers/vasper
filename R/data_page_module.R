# Data Page Module ----

#' Single data viewer module UI
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @export
data_view_module_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("module_ui"))
}

#' Single data viewer module server
#'
#' @param id Module id
#' @param con DBI connection
#' @param tables_r Reactive expression that returns table names
#' @param metadata_r Reactive expression that returns table metadata
#' @param refresh_nonce_r Reactive expression used to refresh data
#' @param remove_cb Callback used to remove this module
#' @param on_select_table_cb Callback when selected table changes
#' @param add_views_cb Callback to add new table views from subchat tool results
#' @param initial_table Optional initial table name for this module
#'
#' @return Invisibly TRUE
#' @export
data_view_module_server <- function(
  id,
  con,
  tables_r,
  metadata_r,
  refresh_nonce_r,
  remove_cb,
  on_select_table_cb,
  add_views_cb = NULL,
  initial_table = NULL
) {
  moduleServer(id, function(input, output, session) {
    selected_table <- reactiveVal(initial_table)
    collapsed <- reactiveVal(TRUE)
    data_chat_observers <- reactiveVal(list())
    data_chat_sessions <- reactiveVal(list())

    local_chat_id_for_table <- function(table_name) {
      key <- gsub("[^A-Za-z0-9_]+", "_", table_name)
      key <- gsub("_+", "_", key)
      key <- gsub("^_|_$", "", key)

      if (!nzchar(key)) {
        key <- "table"
      }

      max_key_chars <- 80L
      if (nchar(key) > max_key_chars) {
        key <- substr(key, 1, max_key_chars)
        key <- gsub("_+$", "", key)
      }

      paste0("data_chat_", key)
    }

    table_label <- reactive({
      tbl <- selected_table()
      req(tbl)

      md <- metadata_r()
      req(nrow(md) > 0)

      match_idx <- match(tbl, md$table_name)
      if (is.na(match_idx)) {
        return(stringr::str_to_title(gsub("_", " ", tbl)))
      }

      md$table_label[[match_idx]]
    })

    table_source <- reactive({
      tbl <- selected_table()
      req(tbl)

      md <- metadata_r()
      req(nrow(md) > 0)

      match_idx <- match(tbl, md$table_name)
      if (is.na(match_idx) || !"source" %in% names(md)) {
        return("unknown")
      }

      md$source[[match_idx]] %||% "unknown"
    })

    table_source_detail <- reactive({
      tbl <- selected_table()
      req(tbl)

      md <- metadata_r()
      req(nrow(md) > 0)

      match_idx <- match(tbl, md$table_name)
      if (is.na(match_idx) || !"source_detail" %in% names(md)) {
        return("")
      }

      md$source_detail[[match_idx]] %||% ""
    })

    observe({
      tables <- tables_r()
      current <- selected_table()

      exists_in_db <- !is.null(current) && DBI::dbExistsTable(con, current)

      if (
        !is.null(current) &&
          length(tables) > 0 &&
          !(current %in% tables) &&
          !isTRUE(exists_in_db)
      ) {
        selected_table(NULL)
      }
    })

    observeEvent(
      selected_table(),
      {
        req(selected_table())
        on_select_table_cb(selected_table())
      },
      ignoreInit = FALSE
    )

    observeEvent(input$toggle_collapse, {
      collapsed(!collapsed())
    })

    observeEvent(input$delete_module, {
      remove_cb()
    })

    observeEvent(input$show_source, {
      tbl <- selected_table()
      req(tbl)

      source <- table_source()
      detail <- table_source_detail()

      showModal(
        modalDialog(
          title = paste("Table source:", tbl),
          tagList(
            tags$div(
              class = "mb-2",
              tags$strong("Source:"),
              tags$span(class = "ms-2", source)
            ),
            if (nzchar(trimws(detail))) {
              tags$pre(
                class = "small",
                style = "white-space: pre-wrap; word-break: break-word;",
                detail
              )
            } else {
              tags$div(
                class = "small text-muted",
                "No source details recorded."
              )
            }
          ),
          footer = modalButton("Close"),
          easyClose = TRUE,
          size = "m"
        )
      )
    })

    output$table_preview <- renderDT(
      {
        refresh_nonce_r()

        tbl <- selected_table()
        req(tbl)

        escaped_tbl <- as.character(DBI::dbQuoteIdentifier(con, tbl))

        DBI::dbGetQuery(
          con,
          paste0("SELECT * FROM ", escaped_tbl, " LIMIT 500")
        )
      },
      options = list(pageLength = 15, scrollX = TRUE)
    )

    output$module_ui <- renderUI({
      selected <- selected_table()
      selected_title <- if (is.null(selected)) {
        "No table selected"
      } else {
        table_label()
      }
      selected_name_badge <- if (is.null(selected)) "No table" else selected

      summary_row_collapsed <- div(
        class = "data-module-summary",
        span(class = "data-module-title", selected_title),
        tags$span(
          class = "badge text-bg-light data-module-label",
          selected_name_badge
        )
      )

      summary_row_expanded <- div(
        class = "data-module-summary",
        span(class = "data-module-title", selected_title),
        tags$span(
          class = "badge text-bg-light data-module-label",
          selected_name_badge
        )
      )

      chat_btn <- if (!is.null(selected)) {
        actionButton(
          session$ns("open_data_chat"),
          label = NULL,
          icon = icon("comments"),
          class = "btn btn-sm btn-outline-primary",
          title = "Chat about this table"
        )
      }

      controls <- div(
        class = "data-module-controls",
        chat_btn,
        actionButton(
          session$ns("toggle_collapse"),
          label = NULL,
          icon = icon(if (collapsed()) "chevron-down" else "chevron-up"),
          class = "btn btn-sm btn-outline-secondary",
          title = if (collapsed()) "Expand" else "Collapse"
        ),
        actionButton(
          session$ns("show_source"),
          label = NULL,
          icon = icon("circle-info"),
          class = "btn btn-sm btn-outline-secondary",
          title = "View source details"
        ),
        actionButton(
          session$ns("delete_module"),
          label = NULL,
          icon = icon("trash"),
          class = "btn btn-sm btn-outline-danger",
          title = "Delete"
        )
      )

      if (collapsed()) {
        return(
          div(
            class = "data-module data-module-collapsed card",
            div(
              class = "card-body py-2 px-3 data-module-header-row",
              summary_row_collapsed,
              controls
            )
          )
        )
      }

      div(
        class = "data-module card",
        div(
          class = "card-header data-module-header-row",
          summary_row_expanded,
          controls
        ),
        div(
          class = "card-body",
          if (!is.null(selected)) {
            DTOutput(session$ns("table_preview"))
          } else {
            div(
              class = "text-muted small mb-2",
              "This table is no longer available. Use 'Manage views' to add or remove views."
            )
          }
        )
      )
    })

    outputOptions(output, "table_preview", suspendWhenHidden = FALSE)

    # -- Ephemeral chat for query_tables data views ----------------------------

    build_data_chat_system_prompt <- function(
      tbl,
      label,
      source_detail,
      column_info,
      sample_rows
    ) {
      col_lines <- if (is.null(column_info) || nrow(column_info) == 0) {
        "(column metadata unavailable)"
      } else {
        paste0(
          "- ",
          column_info$name,
          " (",
          column_info$type,
          ")",
          collapse = "\n"
        )
      }

      paste0(
        "You are a focused data assistant inside a small modal chat.\n",
        "The user is looking at ONE DuckDB table from a previous query.\n",
        "Keep responses short and scannable for mobile:\n",
        "- default: <= 60 words or <= 4 short bullets\n",
        "- include only directly useful details\n",
        "- ask at most one clarification question when necessary\n\n",
        "Table name: ",
        tbl,
        "\n",
        "Label: ",
        label,
        "\n",
        if (nzchar(trimws(source_detail %||% ""))) {
          paste0("Source detail:\n", source_detail, "\n")
        },
        "\nColumns:\n",
        col_lines,
        "\n",
        "\nSample rows (up to 5):\n",
        paste(utils::capture.output(sample_rows), collapse = "\n"),
        "\n",
        "\nYou can:\n",
        "- Run SQL queries with query_tables.\n",
        "- Save query results by setting persist = TRUE and providing output_table_names and output_table_labels.\n",
        "- Persisted results must use NEW table names. Existing tables cannot be replaced.\n",
        "- List schemas with list_plot_schemas, then load selected templates with read_plot_schemas.\n",
        "- If the user asks for a chart or visualization, use escalate_to_main_chat.\n",
        "- If the request goes beyond your tools, use escalate_to_main_chat.\n"
      )
    }

    build_data_chat_tools <- function() {
      tool_list_schemas <- tool(
        function() run_list_plot_schemas(),
        name = "list_plot_schemas",
        description = "List available plot schemas (summary only).",
        arguments = list()
      )

      tool_read_schemas <- tool(
        function(schema_names) {
          run_read_plot_schemas(schema_names = schema_names)
        },
        name = "read_plot_schemas",
        description = "Read full template payload for selected schema names.",
        arguments = list(
          schema_names = type_array(
            type_string(),
            "Schema names selected from list_plot_schemas."
          )
        )
      )

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
          "Send a message to the main chat when this request exceeds your available tools,",
          "including when the user asks for a chart or visualization.",
          "The modal will close automatically."
        ),
        arguments = list(
          message = type_string("The message to send to the main chat.")
        )
      )

      list(
        tool_list_schemas,
        tool_read_schemas,
        tool_query,
        tool_escalate
      )
    }

    show_data_chat <- function(tbl) {
      req(!is.null(tbl))

      label <- table_label()
      detail <- table_source_detail()

      escaped <- as.character(DBI::dbQuoteIdentifier(con, tbl))
      sample_rows <- DBI::dbGetQuery(
        con,
        paste0("SELECT * FROM ", escaped, " LIMIT 5")
      )

      column_info <- tryCatch(
        {
          rs <- DBI::dbSendQuery(
            con,
            paste0("SELECT * FROM ", escaped, " LIMIT 0")
          )
          on.exit(DBI::dbClearResult(rs), add = TRUE)
          info <- DBI::dbColumnInfo(rs)
          info[, c("name", "type"), drop = FALSE]
        },
        error = function(e) {
          NULL
        }
      )

      sessions <- isolate(data_chat_sessions())
      session_key <- tbl
      chat_session <- sessions[[session_key]]

      if (is.null(chat_session)) {
        sys_prompt <- build_data_chat_system_prompt(
          tbl = tbl,
          label = label,
          source_detail = detail,
          column_info = column_info,
          sample_rows = sample_rows
        )
        chat_client <- create_chat_client(sys_prompt)
        chat_client$on_tool_result(function(result) {
          parsed <- parse_tool_result_data_view(result)

          if (!parsed$should_queue || length(parsed$table_names) == 0) {
            return(invisible(NULL))
          }

          if (is.function(add_views_cb)) {
            add_views_cb(parsed$table_names)
          }

          invisible(NULL)
        })

        tools <- build_data_chat_tools()
        registration_ok <- tryCatch(
          {
            chat_client$register_tools(tools)
            TRUE
          },
          error = function(e) {
            showNotification(
              paste0(
                "Unable to initialize data chat tools: ",
                conditionMessage(e)
              ),
              type = "error"
            )
            FALSE
          }
        )
        req(registration_ok)

        chat_session <- list(
          chat_id = local_chat_id_for_table(tbl),
          chat_client = chat_client
        )
        sessions[[session_key]] <- chat_session
        data_chat_sessions(sessions)
      }

      local_chat_id <- chat_session$chat_id
      chat_client <- chat_session$chat_client
      chat_ui_id <- session$ns(local_chat_id)

      showModal(
        modalDialog(
          title = paste0("Chat: ", label %||% tbl),
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

      prev_observers <- isolate(data_chat_observers())
      data_chat_observers(register_subchat_streaming(
        session = session,
        input = input,
        user_input_id = paste0(local_chat_id, "_user_input"),
        chat_id = local_chat_id,
        chat_client = chat_client,
        existing_observers = prev_observers,
        stream_error_prefix = "Streaming error: "
      ))
    }

    observeEvent(input$open_data_chat, {
      tbl <- selected_table()
      req(tbl)
      show_data_chat(tbl)
    })

    invisible(TRUE)
  })
}
