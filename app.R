source("global.R")

chat_welcome_message <- paste0(
  "I'm Vasper, your soil health and weather assistant. ",
  "I can fetch weather forecasts and historical data, navigate ",
  "app pages, and help you explore soil data in your region.",
  "<div class='suggestion'>What's the 7-day forecast for Columbia County?</div>",
  "<div class='suggestion'>Get last year's rainfall for Columbia County</div>",
  "<div class='suggestion'>Summarize wheat yields in spring in Columbia County</div>"
)

# Build hamburger menu links from app_pages registry
hamburger_links <- lapply(names(app_pages), function(key) {
  pg <- app_pages[[key]]
  actionLink(
    paste0("nav_", key),
    label = tagList(icon(pg$icon), pg$title),
    class = "d-block py-1"
  )
})

# Build compact header links (shown as space allows; overflow handled by menu)
header_links <- c(
  list(
    actionLink(
      "hdr_chat",
      label = tagList(icon("comments"), "Chat"),
      class = "app-header-link"
    )
  ),
  lapply(names(app_pages), function(key) {
    pg <- app_pages[[key]]
    actionLink(
      paste0("hdr_", key),
      label = tagList(icon(pg$icon), pg$title),
      class = "app-header-link"
    )
  })
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

  # Header with title + compact navigation menu
  div(
    class = "app-header",
    div(
      class = "app-header-main",
      div(class = "app-header-title", "Vasper"),
      div(
        class = "app-header-links",
        !!!header_links
      )
    ),
    div(
      class = "app-header-menu",
      dropdown(
        actionLink(
          "nav_chat",
          label = tagList(icon("comments"), "Chat"),
          class = "d-block py-1"
        ),
        !!!hamburger_links,
        icon = icon("bars"),
        size = "sm",
        status = "outline-secondary",
        right = TRUE,
        width = "200px"
      )
    )
  ),

  # Top-level: chat vs pages
  navset_hidden(
    id = "app_view",

    # Chat view (default)
    nav_panel_hidden(
      value = "chat",
      chat_ui(
        "main_chat",
        messages = list(
          chat_welcome_message
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
          div(
            style = "max-width: 400px; padding: 1rem;",
            h5("Soil Health Reports"),
            p(
              "Select a producer, year range, and depth, then click the",
              "button",
              "to generate a soil health report. Reports include tables,",
              "strip plots, and maps comparing the producer's fields to",
              "project averages."
            ),
            p(
              class = "text-muted small",
              "HTML reports are interactive (hoverable plots, downloadable",
              "data). Word reports are suitable for printing."
            ),
            p(
              class = "text-muted small",
              "Reports generated with the",
              tags$a(
                "{soils} R package",
                href = "https://wa-department-of-agriculture.github.io/soils/",
                target = "_blank"
              ),
              "developed by WSDA and WSU as part of the",
              tags$a(
                "Washington Soil Health Initiative.",
                href = "https://washingtonsoilhealthinitiative.com/",
                target = "_blank"
              )
            ),
            hr(),
            selectInput(
              "report_producer",
              "Producer",
              choices = NULL,
              width = "100%"
            ),
            sliderInput(
              "report_year_range",
              label = div(
                "Year Range",
                tags$div(
                  class = "form-text text-muted",
                  "Range is based on available years for the selected producer."
                )
              ),
              min = 2000,
              max = 2000,
              value = c(2000, 2000),
              step = 1,
              sep = "",
              width = "100%"
            ),
            selectInput(
              "report_depth",
              "Depth (inches)",
              choices = NULL,
              width = "100%"
            ),
            radioButtons(
              "report_format",
              "Format",
              choices = c("HTML" = "html", "Word" = "docx", "All" = "all"),
              selected = "html"
            ),
            downloadButton("download_report", "Generate & Download Report")
          )
        ),

        # Soil Data
        nav_panel_hidden(
          value = "soil_data",
          card(
            card_header("Soil Chemistry Samples"),
            card_body(
              class = "p-1",
              DTOutput("soil_table")
            )
          )
        )
      )
    )
  ),

  # Bottom bar with toggle link
  div(
    class = "bottom-bar",
    actionLink(
      "scroll_top",
      label = "Top",
      icon = icon("arrow-up")
    ),
    div(
      class = "bottom-bar-center",
      actionLink(
        "toggle_view",
        label = "Hide chat",
        icon = icon("eye-slash"),
        class = "bottom-bar-mid-link"
      ),
      actionLink(
        "clear_chat",
        label = "Clear chat",
        icon = icon("trash"),
        class = "bottom-bar-mid-link"
      )
    ),
    actionLink(
      "scroll_bottom",
      label = "End",
      icon = icon("arrow-down")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # -- Page navigation state --
  active_page <- reactiveVal(NULL) # NULL = chat visible
  last_page <- reactiveVal("reports") # remember last page for toggle

  set_active_nav <- function(page = c("chat", names(app_pages))) {
    page <- match.arg(page)
    session$sendCustomMessage("set-active-page", list(page = page))
  }

  # Helper: navigate to a page
  navigate_to <- function(page_key) {
    active_page(page_key)
    last_page(page_key)
    nav_select("app_view", "pages")
    nav_select("pages_nav", page_key)
    updateActionButton(
      session,
      "toggle_view",
      label = "Show chat",
      icon = icon("comments")
    )
    set_active_nav(page_key)
  }

  # Helper: navigate back to chat
  navigate_to_chat <- function() {
    active_page(NULL)
    set_active_nav("chat")
    session$sendCustomMessage(
      "scroll-chat-bottom",
      list(id = "main_chat", phase = "pre")
    )
    nav_select("app_view", "chat")
    session$onFlushed(
      function() {
        session$sendCustomMessage(
          "scroll-chat-bottom",
          list(id = "main_chat", phase = "after")
        )
      },
      once = TRUE
    )
    updateActionButton(
      session,
      "toggle_view",
      label = "Hide chat",
      icon = icon("eye-slash")
    )
  }

  session$onFlushed(
    function() {
      set_active_nav("chat")
    },
    once = TRUE
  )

  # Helper: scroll active content to top/bottom
  scroll_active_content <- function(edge = c("top", "bottom")) {
    edge <- match.arg(edge)

    # Chat view: keep using chat-specific bottom logic for reliability
    if (is.null(active_page())) {
      if (edge == "bottom") {
        session$sendCustomMessage(
          "scroll-chat-bottom",
          list(id = "main_chat", phase = "after")
        )
      } else {
        session$sendCustomMessage(
          "scroll-to-edge",
          list(id = "main_chat", edge = edge)
        )
      }
      return(invisible(NULL))
    }

    # Feature pages: scroll currently active tab content.
    session$sendCustomMessage(
      "scroll-to-edge",
      list(selector = "#app_view .tab-pane.active", edge = edge)
    )
    invisible(NULL)
  }

  # Toggle link: swap between chat and last-viewed page
  observeEvent(input$toggle_view, {
    if (is.null(active_page())) {
      navigate_to(last_page())
    } else {
      navigate_to_chat()
    }
  })

  observeEvent(input$scroll_top, {
    scroll_active_content("top")
  })

  observeEvent(input$scroll_bottom, {
    scroll_active_content("bottom")
  })

  observeEvent(input$clear_chat, {
    main_chat$set_turns(list())
    chat_clear("main_chat")
    chat_append("main_chat", chat_welcome_message)
    navigate_to_chat()
  })

  # Hamburger menu: chat link
  observeEvent(input$nav_chat, {
    navigate_to_chat()
  })

  # Header links: chat
  observeEvent(input$hdr_chat, {
    navigate_to_chat()
  })

  # Hamburger menu: generate observers for each page
  lapply(names(app_pages), function(key) {
    observeEvent(input[[paste0("nav_", key)]], {
      navigate_to(key)
    })
  })

  # Header links: generate observers for each page
  lapply(names(app_pages), function(key) {
    observeEvent(input[[paste0("hdr_", key)]], {
      navigate_to(key)
    })
  })

  # Client-side optimistic navigation fallback.
  # While server is busy, JS can switch visible panes immediately and queue
  # this event; once processed, align server-side reactive state.
  observeEvent(input$client_nav_request, {
    req(input$client_nav_request$page)
    page <- input$client_nav_request$page
    if (identical(page, "chat")) {
      navigate_to_chat()
      return(invisible(NULL))
    }
    if (page %in% names(app_pages)) {
      navigate_to(page)
    }
    invisible(NULL)
  })

  # show_page tool: registered per-session so it can call navigate_to()
  all_pages <- c("chat", names(app_pages))
  show_page <- tool(
    function(page) {
      if (page == "chat") {
        navigate_to_chat()
        return("Navigated to Chat")
      }
      if (!page %in% names(app_pages)) {
        return(paste(
          "Invalid page. Choose one of:",
          paste(all_pages, collapse = ", ")
        ))
      }
      navigate_to(page)
      paste("Navigated to", app_pages[[page]]$title)
    },
    name = "show_page",
    description = paste(
      "Navigate the app UI to a specific page or back to the chat.",
      "Available pages:",
      paste(
        c(
          "chat (Chat)",
          vapply(
            names(app_pages),
            function(k) {
              paste0(k, " (", app_pages[[k]]$title, ")")
            },
            character(1)
          )
        ),
        collapse = ", "
      )
    ),
    arguments = list(
      page = type_enum(
        "The page to navigate to",
        values = all_pages
      )
    )
  )
  main_chat$register_tool(show_page)

  # -- Existing functionality --

  # Soil Data Table
  soil_data_cache <- reactiveVal(NULL)

  refresh_soil_data_cache <- function() {
    dat <- tryCatch(
      DBI::dbGetQuery(con, "SELECT * FROM soil_data"),
      error = function(e) {
        fallback <- tibble::tibble(
          status = "Soil data temporarily unavailable",
          details = conditionMessage(e)
        )
        fallback
      }
    )
    soil_data_cache(dat)
    invisible(NULL)
  }

  # Load once at startup so the table is ready even if the panel is hidden.
  refresh_soil_data_cache()

  output$soil_table <- renderDT(
    req(soil_data_cache()),
    options = list(pageLength = 15, scrollX = TRUE)
  )

  # Keep output alive when hidden so first reveal does not depend on server timing.
  outputOptions(output, "soil_table", suspendWhenHidden = FALSE)

  # Main AI Chat with Weather Tools
  # NOTE: non-streaming async keeps the Shiny session more responsive to
  # navigation/events than token-by-token streaming in a single callback.
  chat_busy <- reactiveVal(FALSE)

  observeEvent(input$main_chat_user_input, {
    req(!chat_busy())

    chat_busy(TRUE)
    user_message <- input$main_chat_user_input

    main_chat$chat_async(user_message) |>
      promises::then(function(response) {
        chat_append("main_chat", as.character(response))
        NULL
      }) |>
      promises::catch(function(err) {
        chat_append(
          "main_chat",
          paste0(
            "I hit an error while generating a response: ",
            conditionMessage(err)
          )
        )
        NULL
      }) |>
      promises::finally(function() {
        # Refresh cache after tool/chat work completes in case DB writes occurred.
        refresh_soil_data_cache()
        chat_busy(FALSE)
        NULL
      })
  })

  # Populate report dropdowns from the database
  format_year_label <- function(year_range) {
    if (length(year_range) != 2 || any(is.na(year_range))) {
      return(NA_character_)
    }
    start_year <- as.integer(year_range[1])
    end_year <- as.integer(year_range[2])
    if (start_year == end_year) {
      as.character(start_year)
    } else {
      glue::glue("{start_year}-{end_year}")
    }
  }

  observe({
    producers <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT producer_id FROM soil_data ORDER BY producer_id"
    )$producer_id
    updateSelectInput(
      session,
      "report_producer",
      choices = producers,
      selected = "COLUMBIA CONSERVATION DISTRICT"
    )
  })

  observe({
    req(input$report_producer)
    years <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT year FROM soil_data WHERE producer_id = ? ORDER BY year",
      params = list(input$report_producer)
    )$year

    req(length(years) > 0)
    year_min <- min(years, na.rm = TRUE)
    year_max <- max(years, na.rm = TRUE)

    updateSliderInput(
      session,
      "report_year_range",
      min = year_min,
      max = year_max,
      value = c(year_min, year_max),
      step = 1
    )
  })

  observe({
    req(input$report_producer, input$report_year_range)
    req(length(input$report_year_range) == 2)

    year_start <- as.integer(min(input$report_year_range))
    year_end <- as.integer(max(input$report_year_range))

    depths_raw <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT depth FROM soil_data
       WHERE producer_id = ?
         AND year BETWEEN ? AND ?
       ORDER BY depth",
      params = list(input$report_producer, year_start, year_end)
    )$depth

    req(length(depths_raw) > 0)

    updateSelectInput(
      session,
      "report_depth",
      choices = depths_raw,
      selected = depths_raw[1]
    )
  })

  # Download handler for reports
  output$download_report <- downloadHandler(
    filename = function() {
      ext <- if (input$report_format == "all") "zip" else input$report_format
      depth_suffix <- gsub("-", "to", input$report_depth)
      year_label <- format_year_label(input$report_year_range)
      glue::glue(
        "{year_label}_{input$report_producer}_{depth_suffix}_Report.{ext}"
      )
    },
    content = function(file) {
      req(input$report_year_range)
      req(length(input$report_year_range) == 2)

      year_start <- as.integer(min(input$report_year_range))
      year_end <- as.integer(max(input$report_year_range))

      # Show a notification while rendering
      id <- showNotification(
        "Rendering report... this may take a moment.",
        duration = NULL,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)

      report_path <- render_report(
        producer_id = input$report_producer,
        year_start = year_start,
        year_end = year_end,
        depth = input$report_depth,
        format = input$report_format,
        con = con,
        data_table = "soil_data",
        dictionary_table = "data_dictionary",
        output_dir = dirname(file)
      )

      # Rename into the path Shiny expects for the download.
      if (!file.rename(report_path, file)) {
        stop("Failed to deliver the rendered report.")
      }
    }
  )
}

# Run App ----
shinyApp(ui = ui, server = server)
