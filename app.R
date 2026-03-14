source("global.R")

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
    tags$script(src = "js/scroll.js")
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
          paste0(
            "I'm Vasper, your soil health and weather assistant. ",
            "I can fetch weather forecasts and historical data, navigate ",
            "app pages, and help you explore soil data in your region.",
            "<div class='suggestion'>What's the 7-day forecast for Columbia County?</div>",
            "<div class='suggestion'>Get last year's rainfall for Columbia County</div>",
            "<div class='suggestion'>Summarize wheat yields in spring in Columbia County</div>"
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
          div(
            style = "max-width: 400px; padding: 1rem;",
            h5("Soil Health Reports"),
            p(
              "Select a producer, year, and depth, then click the button",
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
            selectInput(
              "report_year",
              "Year",
              choices = NULL,
              width = "100%"
            ),
            selectInput(
              "report_depth",
              "Depth",
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
    actionLink(
      "toggle_view",
      label = "Hide chat",
      icon = icon("eye-slash")
    ),
    actionLink(
      "scroll_bottom",
      label = "Bottom",
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
  output$soil_table <- renderDT(
    DBI::dbGetQuery(con, "SELECT * FROM soil_data"),
    options = list(pageLength = 15, scrollX = TRUE)
  )

  # Main AI Chat with Weather Tools
  observeEvent(input$main_chat_user_input, {
    stream <- main_chat$stream_async(input$main_chat_user_input)
    chat_append("main_chat", stream)
  })

  # Populate report dropdowns from the database
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
    updateSelectInput(
      session,
      "report_year",
      choices = years,
      selected = max(years)
    )
  })

  observe({
    req(input$report_producer, input$report_year)
    depths_raw <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT depth FROM soil_data
       WHERE producer_id = ? AND year = ?
       ORDER BY depth",
      params = list(input$report_producer, as.integer(input$report_year))
    )$depth
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
      glue::glue(
        "{input$report_year}_{input$report_producer}_{depth_suffix}_Report.{ext}"
      )
    },
    content = function(file) {
      # Show a notification while rendering
      id <- showNotification(
        "Rendering report... this may take a moment.",
        duration = NULL,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)

      report_path <- render_report(
        producer_id = input$report_producer,
        year = as.integer(input$report_year),
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
