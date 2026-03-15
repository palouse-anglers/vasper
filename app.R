source("global.R")

if (!("reports-images" %in% names(shiny::resourcePaths()))) {
  shiny::addResourcePath("reports-images", "reports/images")
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
    tags$link(rel = "stylesheet", href = "css/app.css"),
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
        chat_ui(
          "main_chat",
          messages = list(
            chat_welcome_message
          )
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
  navigation_api <- navigation_server(
    input = input,
    output = output,
    session = session,
    main_chat = main_chat,
    chat_welcome_message = chat_welcome_message,
    app_pages = app_pages,
    chat_id = "main_chat"
  )

  # -- Existing functionality --

  # Data page modules refresh off this nonce whenever chat/tools modify DB.
  data_refresh_nonce <- reactiveVal(0)

  data_page_api <- data_page_manager_server(
    id = "data_page",
    con = con,
    refresh_nonce_r = reactive(data_refresh_nonce()),
    include_tables = c(TABLE_NAMES$table_metadata),
    ignore_tables = character()
  )

  get_data_table_metadata <- tool(
    function() {
      md <- data_page_api$get_metadata()
      if (nrow(md) == 0) {
        return(list(tables = list()))
      }

      list(
        tables = purrr::transpose(as.list(md))
      )
    },
    name = "get_data_table_metadata",
    description = paste(
      "Get available data tables from table_metadata with minimal details",
      "(table_name, table_label, row_count, column_count)."
    )
  )

  main_chat$register_tool(get_data_table_metadata)

  # Auto-open Data views when tools return add_data_view = TRUE and a table_name.
  main_chat$on_tool_result(function(result) {
    deep_extract <- function(x, key) {
      out <- list()
      if (is.list(x)) {
        if (!is.null(names(x)) && key %in% names(x)) {
          out <- c(out, list(x[[key]]))
        }
        for (el in x) {
          out <- c(out, deep_extract(el, key))
        }
      }
      out
    }

    add_flags <- unlist(
      deep_extract(result, "add_data_view"),
      use.names = FALSE
    )
    table_names <- unlist(deep_extract(result, "table_name"), use.names = FALSE)
    table_names <- table_names[nzchar(table_names)]

    add_flags <- suppressWarnings(as.logical(add_flags))

    if (!any(add_flags, na.rm = TRUE) || length(table_names) == 0) {
      return(invisible(NULL))
    }

    # Refresh metadata first so newly created tables are available to add_views.
    data_refresh_nonce(data_refresh_nonce() + 1)
    data_page_api$add_views(unique(table_names))
    data_refresh_nonce(data_refresh_nonce() + 1)
    invisible(NULL)
  })

  # Fallback path: consume tool-requested Data views from global queue.
  observe({
    invalidateLater(250, session)

    req(exists("data_view_queue", inherits = TRUE))
    queued <- get("data_view_queue", inherits = TRUE)
    table_names <- unique(queued$table_names)

    if (length(table_names) == 0) {
      return(invisible(NULL))
    }

    queued$table_names <- character()

    data_refresh_nonce(data_refresh_nonce() + 1)
    data_page_api$add_views(table_names)
    data_refresh_nonce(data_refresh_nonce() + 1)

    invisible(NULL)
  })

  # Main AI Chat with Weather Tools
  # Use content streaming so shinychat can render tool calls inline.
  observeEvent(input$main_chat_user_input, {
    stream <- main_chat$stream_async(
      input$main_chat_user_input,
      stream = "content"
    )
    chat_append("main_chat", stream)
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
