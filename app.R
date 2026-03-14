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

ui <- page_fillable(
  title = "Vasper",
  theme = app_theme,
  padding = 0,

  tags$head(tags$style(HTML(
    "
    /* Bottom bar: toggle link */
    .bottom-bar {
      border-top: 1px solid var(--bs-border-color, #dee2e6);
      padding: 0.4rem 0.75rem;
      text-align: center;
      background: var(--bs-body-bg, white);
    }
    /* Hamburger button */
    .hamburger-btn {
      position: fixed;
      top: 8px;
      right: 8px;
      z-index: 200;
    }
    /* Footer: sticky to viewport bottom */
    .bottom-bar {
      position: fixed;
      bottom: 0;
      left: 0;
      right: 0;
      z-index: 200;
      border-top: 1px solid var(--bs-border-color, #dee2e6);
      padding: 0.4rem 0.75rem;
      text-align: center;
      background: var(--bs-body-bg, white);
    }
    /* Shrink fill container so content stops above the footer */
    html { height: calc(100vh - 2.5rem); }
  "
  ))),

  # Hamburger menu (fixed top-right)
  div(
    class = "hamburger-btn",
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
  ),

  # Top-level: chat vs pages
  navset_hidden(
    id = "app_view",

    # Chat view (default)
    nav_panel_hidden(
      value = "chat",
      chat_ui("main_chat")
    ),

    # Pages view
    nav_panel_hidden(
      value = "pages",
      navset_pill(
        id = "pages_nav",

        # Reports
        nav_panel(
          title = "Reports",
          icon = icon("file-alt"),
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
        nav_panel(
          title = "Soil Data",
          icon = icon("flask"),
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
      "toggle_view",
      label = "Hide chat",
      icon = icon("eye-slash")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # -- Page navigation state --
  active_page <- reactiveVal(NULL) # NULL = chat visible
  last_page <- reactiveVal("reports") # remember last page for toggle

  # Helper: navigate to a page
  navigate_to <- function(page_key) {
    active_page(page_key)
    last_page(page_key)
    nav_select("app_view", "pages")
    nav_select("pages_nav", app_pages[[page_key]]$title)
    updateActionButton(
      session,
      "toggle_view",
      label = "Show chat",
      icon = icon("comments")
    )
  }

  # Helper: navigate back to chat
  navigate_to_chat <- function() {
    active_page(NULL)
    nav_select("app_view", "chat")
    updateActionButton(
      session,
      "toggle_view",
      label = "Hide chat",
      icon = icon("eye-slash")
    )
  }

  # Toggle link: swap between chat and last-viewed page
  observeEvent(input$toggle_view, {
    if (is.null(active_page())) {
      navigate_to(last_page())
    } else {
      navigate_to_chat()
    }
  })

  # Hamburger menu: chat link
  observeEvent(input$nav_chat, {
    navigate_to_chat()
  })

  # Hamburger menu: generate observers for each page
  lapply(names(app_pages), function(key) {
    observeEvent(input[[paste0("nav_", key)]], {
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
