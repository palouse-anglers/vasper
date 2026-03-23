source("global.R")

if (!("reports-images" %in% names(shiny::resourcePaths()))) {
  shiny::addResourcePath("reports-images", "reports/images")
}

if (!("artifact-plots" %in% names(shiny::resourcePaths()))) {
  shiny::addResourcePath("artifact-plots", get_plot_artifact_base_dir("plots"))
}

acknowledgements_banner <- tags$div(
  class = "app-acknowledgements",
  htmltools::HTML(paste0(
    "<span>Acknowledgements:</span> Data services and tooling include ",
    "<a href='https://open-meteo.com/' target='_blank' rel='noopener noreferrer'>Open-Meteo</a>, ",
    "<a href='https://weatherlink.github.io/v2-api/' target='_blank' rel='noopener noreferrer'>Davis WeatherLink</a>, ",
    "<a href='https://quickstats.nass.usda.gov/api' target='_blank' rel='noopener noreferrer'>USDA NASS Quick Stats API</a>, and the ",
    "<a href='https://wa-department-of-agriculture.github.io/soils/' target='_blank' rel='noopener noreferrer'>{soils} R package</a>.",
    "<br/>This product uses the NASS API but is not endorsed or certified by NASS."
  ))
)

ui <- page_fillable(
  title = "Vasper",
  theme = app_theme,
  padding = 0,

  tags$head(
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = "icons/vasper-logo.svg"
    ),
    tags$link(rel = "stylesheet", href = "css/app.css"),
    tags$style(HTML(sprintf(
      "
      .shiny-notification,
      .shiny-notification-message {
        border-left: 4px solid %s !important;
        background-color: %s !important;
        color: %s !important;
      }
      .shiny-notification-warning {
        border-left-color: %s !important;
        background-color: %s !important;
      }
      .shiny-notification-error {
        border-left-color: %s !important;
        background-color: %s !important;
      }
    ",
      BRAND_COLORS$primary,
      BRAND_COLORS$primary_light,
      BRAND_COLORS$foreground,
      BRAND_COLORS$warning,
      BRAND_COLORS$warning_light,
      BRAND_COLORS$danger,
      BRAND_COLORS$danger_light
    ))),
    tags$script(src = "js/scroll.js"),
    tags$script(src = "js/navigation.js")
  ),

  navigation_header_ui(app_pages = app_pages, title = "Vasper"),

  # Top-level: chat vs pages
  navset_hidden(
    id = "app_view",

    # Chat view (default)
    nav_panel_hidden(
      value = "chat",
      tags$div(
        class = "chat-view-container",
        acknowledgements_banner,
        tags$div(
          class = "chat-profile-toolbar",
          uiOutput("prompt_profile_context")
        ),
        chat_ui(
          "main_chat",
          messages = list()
        )
      )
    ),

    # Pages view
    nav_panel_hidden(
      value = "pages",
      navset_hidden(
        id = "pages_nav",

        # Reports
        nav_panel_hidden(
          value = "reports",
          report_controls_ui("report_controls")
        ),

        # Visual artifacts
        nav_panel_hidden(
          value = "visuals",
          visual_artifacts_ui("visual_artifacts")
        ),

        # Data
        nav_panel_hidden(
          value = "data",
          data_page_manager_ui("data_page")
        )
      )
    )
  ),

  navigation_bottom_bar_ui()
)

# Server ----
server <- function(input, output, session) {
  artifact_plot_dir <- normalizePath(
    get_plot_artifact_base_dir("plots"),
    winslash = "/",
    mustWork = FALSE
  )

  # Data page modules refresh off this nonce whenever chat/tools modify DB.
  data_refresh_nonce <- reactiveVal(0)

  data_page_api <- data_page_manager_server(
    id = "data_page",
    con = con,
    refresh_nonce_r = reactive(data_refresh_nonce()),
    include_tables = c(TABLE_NAMES$table_metadata),
    ignore_tables = character()
  )

  visual_artifacts_server(
    id = "visual_artifacts",
    con = con,
    refresh_nonce_r = reactive(data_refresh_nonce()),
    on_refresh_cb = function() {
      data_refresh_nonce(data_refresh_nonce() + 1)
    }
  )

  tool_get_table_metadata <- tool(
    function(
      include_source = FALSE,
      table_pattern = NULL,
      table_names = NULL
    ) {
      md <- data_page_api$get_metadata(include_source = isTRUE(include_source))

      if (!is.null(table_pattern) && length(table_pattern) > 0) {
        pattern <- as.character(table_pattern[[1]])
        pattern <- trimws(pattern)

        if (nzchar(pattern)) {
          matches <- grepl(pattern, md$table_name, ignore.case = TRUE)
          md <- md[matches, , drop = FALSE]
        }
      }

      if (!is.null(table_names) && length(table_names) > 0) {
        table_names <- unlist(table_names, recursive = TRUE, use.names = FALSE)
        table_names <- as.character(table_names)
        table_names <- trimws(table_names)
        table_names <- table_names[nzchar(table_names)]

        if (length(table_names) > 0) {
          md <- md[md$table_name %in% table_names, , drop = FALSE]
        }
      }

      if (nrow(md) == 0) {
        return(list(tables = list()))
      }

      md$column_names <- lapply(md$table_name, function(tbl) {
        tryCatch(
          DBI::dbListFields(con, tbl),
          error = function(e) character()
        )
      })

      if (!isTRUE(include_source)) {
        keep <- c(
          "table_name",
          "table_label",
          "row_count",
          "column_count",
          "column_names"
        )
        keep <- intersect(keep, names(md))
        md <- md[, keep, drop = FALSE]
      }

      list(
        tables = purrr::transpose(as.list(md))
      )
    },
    name = "get_table_metadata",
    description = paste(
      "Get available tables from table_metadata with minimal details",
      "(table_name, table_label, row_count, column_count, column_names).",
      "Use include_source=TRUE to include source and source_detail provenance only when you need it for SQL.",
      "Supports table_pattern regex matching and exact table_names filtering."
    ),
    arguments = list(
      include_source = type_boolean(
        "Include source and source_detail provenance fields (default FALSE)",
        required = FALSE
      ),
      table_pattern = type_string(
        "Optional regex pattern to filter table_name values",
        required = FALSE
      ),
      table_names = type_array(
        type_string(),
        "Optional exact table_name filter list",
        required = FALSE
      )
    ),
    annotations = tool_annotations(
      title = "Table Metadata",
      icon = icon("table")
    )
  )

  pending_data_views <- reactiveVal(character())
  pending_plot_messages <- reactiveVal(character())

  queue_data_views <- function(table_names) {
    table_names <- normalize_table_names(table_names)

    if (length(table_names) == 0) {
      return(invisible(NULL))
    }

    pending_data_views(unique(c(isolate(pending_data_views()), table_names)))
    invisible(NULL)
  }

  queue_plot_message <- function(message) {
    message <- sanitize_chat_text_scalar(message)

    if (is.na(message)) {
      return(invisible(NULL))
    }

    pending_plot_messages(c(isolate(pending_plot_messages()), message))
    invisible(NULL)
  }

  to_artifact_plot_url <- function(png_path) {
    png_path <- sanitize_chat_text_scalar(png_path)
    if (is.na(png_path)) {
      return(NA_character_)
    }

    normalized_path <- normalizePath(
      png_path,
      winslash = "/",
      mustWork = FALSE
    )

    if (!file.exists(normalized_path)) {
      return(NA_character_)
    }

    relative <- if (startsWith(normalized_path, artifact_plot_dir)) {
      sub(
        "^/+",
        "",
        substr(
          normalized_path,
          nchar(artifact_plot_dir) + 1,
          nchar(normalized_path)
        )
      )
    } else {
      basename(normalized_path)
    }

    parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
    encoded <- paste(
      vapply(parts, utils::URLencode, character(1), reserved = TRUE),
      collapse = "/"
    )

    modified <- as.numeric(file.info(normalized_path)$mtime)
    paste0("artifact-plots/", encoded, "?t=", as.integer(modified))
  }

  build_plot_chat_message <- function(result) {
    parsed <- parse_tool_result_plot_artifact(result)

    if (!parsed$has_plot) {
      return(NA_character_)
    }

    plot_url <- to_artifact_plot_url(parsed$png_path)
    if (is.na(plot_url)) {
      return(NA_character_)
    }

    label <- sanitize_chat_text_scalar(parsed$artifact_label)
    if (is.na(label)) {
      label <- sanitize_chat_text_scalar(parsed$artifact_id)
    }
    if (is.na(label)) {
      label <- "Generated plot"
    }

    paste0(
      "**",
      label,
      "**\n\n",
      "![",
      label,
      "](",
      plot_url,
      ")"
    )
  }

  collect_plot_messages_from_turns <- function(turns) {
    if (!is.list(turns) || length(turns) == 0) {
      return(character())
    }

    messages <- character()

    for (turn in turns) {
      tool_results <- extract_turn_tool_results(turn)

      if (length(tool_results) == 0) {
        next
      }

      for (result in tool_results) {
        message <- build_plot_chat_message(result)
        if (!is.na(message)) {
          messages <- c(messages, message)
        }
      }
    }

    messages
  }

  # Auto-open Data views when tools return add_data_view = TRUE.
  handle_tool_result <- function(result) {
    parsed <- parse_tool_result_data_view(result)

    if (!parsed$should_queue || length(parsed$table_names) == 0) {
      return(invisible(NULL))
    }

    queue_data_views(parsed$table_names)
    invisible(NULL)
  }

  handle_tool_plot_result <- function(result) {
    message <- build_plot_chat_message(result)

    if (is.na(message)) {
      return(invisible(NULL))
    }

    queue_plot_message(message)
    invisible(NULL)
  }

  chat_state <- reactiveValues(
    live_chat = NULL,
    archived_threads = list(),
    previous_prompt_id = NULL,
    counter = 0
  )

  is_chat_streaming <- reactiveVal(FALSE)
  stream_tracking <- reactiveValues(
    chat_id = NULL,
    assistant_turn_count = 0
  )
  navigation_api <- list(set_action_enabled = function(...) invisible(NULL))

  set_chat_streaming <- function(value) {
    value <- isTRUE(value)
    is_chat_streaming(value)
    navigation_api$set_action_enabled("clear_chat", enabled = !value)
    navigation_api$set_action_enabled("toggle_view", enabled = !value)
    navigation_api$set_action_enabled("scroll_top", enabled = !value)
    navigation_api$set_action_enabled("scroll_bottom", enabled = !value)
    invisible(value)
  }

  make_chat_id <- function() {
    isolate({
      chat_state$counter <- chat_state$counter + 1
      paste0("chat_", sprintf("%03d", chat_state$counter))
    })
  }

  active_chat <- reactive({
    req(chat_state$live_chat)
    chat_state$live_chat
  })

  active_prompt_id <- reactive({
    req(chat_state$live_chat)
    chat_state$live_chat$prompt_id
  })

  apply_live_chat_to_ui <- function() {
    entry <- isolate(active_chat())
    turns <- entry$client$get_turns()

    if (length(turns) == 0) {
      chat_clear("main_chat")
      chat_append("main_chat", entry$welcome_message)
      return(invisible(NULL))
    }

    render_chat_turns(turns, chat_id = "main_chat")

    replay_plot_messages <- collect_plot_messages_from_turns(turns)
    if (length(replay_plot_messages) > 0) {
      for (message in replay_plot_messages) {
        chat_append(
          "main_chat",
          list(role = "assistant", content = message)
        )
      }
    }

    invisible(NULL)
  }

  create_live_chat_entry <- function(prompt_id) {
    prompt_profile <- get_prompt_profile(prompt_config, prompt_id)
    chat_client <- create_chat_client(prompt_profile$system_prompt)
    chat_id <- make_chat_id()

    register_prompt_tools(
      chat_client = chat_client,
      prompt_profile = prompt_profile,
      extra_tools = list(
        get_table_metadata = tool_get_table_metadata,
        show_page = navigation_api$show_page_tool
      )
    )

    chat_client$on_tool_result(function(result) {
      handle_tool_result(result)
      handle_tool_plot_result(result)
    })

    list(
      chat_id = chat_id,
      prompt_id = prompt_profile$id,
      prompt_name = prompt_profile$name,
      thread_base_name = prompt_profile$name,
      mode_history = c(prompt_profile$name),
      resume_index = 0,
      welcome_message = prompt_profile$chat_welcome_message,
      client = chat_client,
      created_at = Sys.time(),
      last_message_at = NULL,
      resume_source_archive_id = NULL,
      resume_source_fingerprint = NULL
    )
  }

  archive_entry <- function(entry, archived_at = Sys.time()) {
    archive_id <- paste0(
      "archive_",
      entry$chat_id,
      "_",
      format(archived_at, "%Y%m%d%H%M%S")
    )

    entry$archived_at <- archived_at
    entry$first_user_preview <- first_user_preview(entry)
    entry$display_name <- thread_display_name(entry)

    archived <- isolate(chat_state$archived_threads)
    archived[[archive_id]] <- entry
    chat_state$archived_threads <- archived

    invisible(archive_id)
  }

  initialize_live_chat <- function(prompt_id = default_prompt_id) {
    chat_state$live_chat <- create_live_chat_entry(prompt_id)
    apply_live_chat_to_ui()
    invisible(chat_state$live_chat$chat_id)
  }

  switch_prompt_profile <- function(prompt_id) {
    req(chat_state$live_chat)

    current <- isolate(chat_state$live_chat)
    if (identical(current$prompt_id, prompt_id)) {
      return(invisible(current$chat_id))
    }

    prompt_profile <- get_prompt_profile(prompt_config, prompt_id)
    existing_turns <- current$client$get_turns()

    # If there is no transcript yet, switch mode in-place.
    if (length(existing_turns) == 0) {
      prior_prompt_id <- current$prompt_id
      current$prompt_id <- prompt_profile$id
      current$prompt_name <- prompt_profile$name
      current$welcome_message <- prompt_profile$chat_welcome_message
      current$mode_history <- c(prompt_profile$name)
      current$thread_base_name <- prompt_profile$name
      chat_state$previous_prompt_id <- prior_prompt_id
      chat_state$live_chat <- current
      apply_live_chat_to_ui()
      return(invisible(current$chat_id))
    }

    # Between messages, branch to a new chat with concatenated mode history.
    if (should_archive_entry(current)) {
      archive_entry(current)
    }

    branched <- create_live_chat_entry(prompt_profile$id)
    branched$client$set_turns(existing_turns)
    current_history <- current$mode_history %||% c(current$prompt_name)
    branched$mode_history <- c(current_history, prompt_profile$name)
    branched$thread_base_name <- paste(branched$mode_history, collapse = "+")
    branched$last_message_at <- current$last_message_at

    chat_state$previous_prompt_id <- current$prompt_id
    chat_state$live_chat <- branched
    apply_live_chat_to_ui()
    invisible(branched$chat_id)
  }

  archive_and_reset_live_chat <- function(prompt_id = NULL) {
    req(chat_state$live_chat)

    current <- isolate(chat_state$live_chat)
    target_prompt_id <- prompt_id %||% current$prompt_id

    if (should_archive_entry(current)) {
      archive_entry(current)
    }

    initialize_live_chat(prompt_id = target_prompt_id)
    invisible(NULL)
  }

  resume_archived_thread <- function(archive_id, clone_archived = TRUE) {
    archived <- isolate(chat_state$archived_threads)
    req(archive_id %in% names(archived))

    current <- isolate(chat_state$live_chat)
    if (!is.null(current) && should_archive_entry(current)) {
      archive_entry(current)
    }

    selected <- archived[[archive_id]]
    prompt_profile <- get_prompt_profile(prompt_config, selected$prompt_id)

    resumed <- create_live_chat_entry(prompt_profile$id)
    # Keep full transcript for LLM continuity (including tool_use/tool_result
    # pairs). UI replay is filtered separately in render_chat_turns().
    selected_turns <- selected$client$get_turns()
    resumed$client$set_turns(selected_turns)
    resumed$created_at <- selected$created_at
    resumed$last_message_at <- selected$last_message_at %||%
      selected$archived_at
    resumed$thread_base_name <- selected$thread_base_name %||%
      selected$prompt_name
    resumed$mode_history <- selected$mode_history %||% c(selected$prompt_name)
    resumed$resume_index <- as.integer(selected$resume_index %||% 0) + 1
    resumed$resume_source_archive_id <- archive_id
    resumed$resume_source_fingerprint <- digest::digest(
      selected_turns,
      algo = "md5"
    )

    chat_state$previous_prompt_id <- if (!is.null(current)) {
      current$prompt_id
    } else {
      NULL
    }
    chat_state$live_chat <- resumed

    if (!clone_archived) {
      archived[[archive_id]] <- NULL
      chat_state$archived_threads <- archived
    }

    apply_live_chat_to_ui()
    invisible(resumed$chat_id)
  }

  clear_active_chat <- function() {
    archive_and_reset_live_chat()
    invisible(NULL)
  }

  ensure_not_streaming <- function(action_label = "This action") {
    if (!isTRUE(isolate(is_chat_streaming()))) {
      return(TRUE)
    }

    showNotification(
      paste0(action_label, " is unavailable while the assistant is streaming."),
      type = "message",
      duration = 2
    )

    FALSE
  }

  navigation_api <- navigation_server(
    input = input,
    output = output,
    session = session,
    app_pages = app_pages,
    chat_id = "main_chat",
    on_clear_chat = clear_active_chat
  )

  set_chat_streaming(isolate(is_chat_streaming()))

  output$prompt_profile_context <- renderUI({
    live <- chat_state$live_chat
    req(!is.null(live))

    tags$div(
      class = "chat-profile-context-wrap",
      actionLink(
        inputId = "new_chat_mode",
        label = tagList(
          tags$span(tags$strong("New chat")),
          icon("plus")
        ),
        class = "chat-profile-context text-muted small"
      ),
      actionLink(
        inputId = "choose_prompt_mode",
        label = tagList(
          tags$span(tags$strong(live$prompt_name)),
          icon("chevron-down")
        ),
        class = "chat-profile-context text-muted small"
      ),
      actionLink(
        inputId = "choose_resume_chat",
        label = tagList(
          tags$span(tags$strong("Previous chats")),
          icon("chevron-down")
        ),
        class = "chat-profile-context text-muted small"
      )
    )
  })

  show_prompt_mode_modal <- function(title = "Choose prompt mode") {
    profiles <- prompt_config$prompts
    if (length(profiles) == 0) {
      return(invisible(NULL))
    }

    selected_prompt_id <- isolate(active_prompt_id())

    rows <- lapply(profiles, function(profile) {
      is_active <- identical(profile$id, selected_prompt_id)

      tags$button(
        type = "button",
        class = paste(
          "btn btn-sm w-100 text-start prompt-mode-row",
          if (is_active) {
            "btn-success"
          } else {
            "btn-outline-secondary"
          }
        ),
        onclick = sprintf(
          "Shiny.setInputValue('prompt_mode_select', '%s', {priority: 'event'})",
          profile$id
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center gap-2",
          tags$span(class = "fw-semibold", profile$name),
          if (is_active) tags$span(class = "small", "Current")
        )
      )
    })

    showModal(
      modalDialog(
        title = title,
        tags$div(class = "d-grid gap-2", !!!rows),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )

    invisible(NULL)
  }

  show_resume_modal <- function() {
    archived <- isolate(chat_state$archived_threads)

    if (length(archived) == 0) {
      showNotification("No archived chats yet.", type = "warning")
      return(invisible(NULL))
    }

    entries <- archived[order(
      vapply(
        archived,
        function(x) as.numeric(x$archived_at %||% x$created_at),
        numeric(1)
      ),
      decreasing = TRUE
    )]

    rows <- lapply(names(entries), function(id) {
      entry <- entries[[id]]
      start_time <- entry$created_at
      last_time <- entry_last_message_time(entry)
      preview <- entry$first_user_preview %||% first_user_preview(entry)
      display_name <- entry$display_name %||% thread_display_name(entry)

      tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-secondary w-100 text-start resume-chat-row",
        onclick = sprintf(
          "Shiny.setInputValue('resume_thread_select', '%s', {priority: 'event'})",
          id
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center gap-2",
          tags$span(
            class = "fw-semibold",
            display_name
          ),
          tags$span(
            class = "text-muted small",
            format_timestamp(entry$archived_at %||% entry$created_at)
          )
        ),
        tags$div(
          class = "small text-muted mt-1",
          paste0(
            "Start: ",
            format_timestamp(start_time),
            " • Last: ",
            format_timestamp(last_time)
          )
        ),
        tags$div(class = "small text-muted mt-1", preview)
      )
    })

    showModal(
      modalDialog(
        title = "Previous chats",
        tags$div(class = "d-grid gap-2", !!!rows),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l"
      )
    )

    invisible(NULL)
  }

  observeEvent(input$choose_prompt_mode, {
    req(ensure_not_streaming("Switch mode"))
    show_prompt_mode_modal(title = "Switch mode in current chat")
  })

  observeEvent(input$new_chat_mode, {
    req(ensure_not_streaming("Starting a new chat"))
    clear_active_chat()
    navigation_api$navigate_to_chat()
  })

  observeEvent(input$prompt_mode_select, {
    req(input$prompt_mode_select)
    req(ensure_not_streaming("Changing mode"))
    removeModal()

    switch_prompt_profile(input$prompt_mode_select)

    navigation_api$navigate_to_chat()
  })

  observeEvent(input$choose_resume_chat, {
    req(ensure_not_streaming("Resuming a chat"))
    show_resume_modal()
  })

  observeEvent(input$resume_thread_select, {
    req(input$resume_thread_select)
    req(ensure_not_streaming("Resuming a chat"))
    removeModal()
    resume_archived_thread(input$resume_thread_select, clone_archived = TRUE)
    navigation_api$navigate_to_chat()
  })

  isolate({
    initialize_live_chat(default_prompt_id)
  })

  # Consume tool-requested Data views from a session-scoped queue.
  flush_data_view_queue <- function() {
    table_names <- normalize_table_names(isolate(pending_data_views()))

    if (length(table_names) == 0) {
      return(invisible(NULL))
    }

    pending_data_views(character())

    data_refresh_nonce(data_refresh_nonce() + 1)
    add_result <- tryCatch(
      data_page_api$add_views(table_names),
      error = function(e) {
        pending_data_views(unique(c(
          isolate(pending_data_views()),
          table_names
        )))
        NULL
      }
    )

    unknown_tables <- normalize_table_names(
      add_result$unknown_tables %||% character()
    )
    if (length(unknown_tables) > 0) {
      pending_data_views(unique(c(
        isolate(pending_data_views()),
        unknown_tables
      )))
    }

    data_refresh_nonce(data_refresh_nonce() + 1)

    invisible(NULL)
  }

  observe({
    table_names <- normalize_table_names(pending_data_views())
    if (length(table_names) == 0) {
      return(invisible(NULL))
    }

    invalidateLater(250, session)
    flush_data_view_queue()
    invisible(NULL)
  })

  observe({
    messages <- pending_plot_messages()

    if (length(messages) == 0) {
      return(invisible(NULL))
    }

    if (isTRUE(is_chat_streaming())) {
      invalidateLater(250, session)
      return(invisible(NULL))
    }

    pending_plot_messages(character())

    for (message in messages) {
      tryCatch(
        chat_append(
          "main_chat",
          list(role = "assistant", content = message)
        ),
        error = function(e) {
          showNotification(
            paste0("Unable to append plot preview: ", conditionMessage(e)),
            type = "warning",
            duration = 3
          )
        }
      )
    }

    invisible(NULL)
  })

  observe({
    if (!isTRUE(is_chat_streaming())) {
      return(invisible(NULL))
    }

    invalidateLater(250, session)

    live <- chat_state$live_chat
    if (is.null(live)) {
      return(invisible(NULL))
    }

    tracked_chat_id <- isolate(stream_tracking$chat_id)
    if (!identical(live$chat_id, tracked_chat_id)) {
      return(invisible(NULL))
    }

    baseline_assistant_count <- isolate(stream_tracking$assistant_turn_count)
    current_assistant_count <- get_assistant_turn_count(live)

    if (current_assistant_count > baseline_assistant_count) {
      set_chat_streaming(FALSE)
    }

    invisible(NULL)
  })

  # Main AI Chat with Weather Tools
  # Use content streaming so shinychat can render tool calls inline.
  observeEvent(input$main_chat_user_input, {
    req(ensure_not_streaming("Sending another message"))

    user_input <- sanitize_chat_text_scalar(input$main_chat_user_input)
    req(!is.na(user_input))

    chat_state$live_chat$last_message_at <- Sys.time()

    stream <- tryCatch(
      active_chat()$client$stream_async(
        user_input,
        tool_mode = "sequential",
        stream = "content"
      ),
      error = function(e) {
        showNotification(
          paste0("Unable to start streaming response: ", conditionMessage(e)),
          type = "error"
        )
        NULL
      }
    )
    req(!is.null(stream))

    live <- isolate(active_chat())
    stream_tracking$chat_id <- live$chat_id
    stream_tracking$assistant_turn_count <- get_assistant_turn_count(live)

    set_chat_streaming(TRUE)

    tryCatch(
      chat_append("main_chat", stream),
      error = function(e) {
        set_chat_streaming(FALSE)
        showNotification(
          paste0("Unable to append streaming response: ", conditionMessage(e)),
          type = "error"
        )
      }
    )
  })

  report_controls_server(
    id = "report_controls",
    con = con,
    data_table = TABLE_NAMES$soil_data,
    dictionary_table = TABLE_NAMES$data_dictionary
  )
}

# Run App ----
shinyApp(ui = ui, server = server)
