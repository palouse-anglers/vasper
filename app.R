source("global.R")

ui <- page_navbar(
  title = "Vasper",
  theme = app_theme,
  fillable = TRUE,

  # Main Chat Interface Tab
  nav_panel(
    title = "AI Assistant",
    icon = icon("comments"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Chat with AI Assistant",
        width = 400,
        chat_mod_ui("main_chat")
      ),
      card(
        card_header("Conversation"),
        "Ask questions about weather, soil data, or yield predictions."
      )
    )
  ),

  # Soil Data Tab
  nav_panel(
    title = "Soil Data",
    icon = icon("flask"),
    card(
      card_header("Soil Chemistry Samples"),
      p("Ask the AI Assistant about soil data, or browse the table below."),
      DTOutput("soil_table")
    )
  ),

  # Reports Tab
  nav_panel(
    title = "Reports",
    icon = icon("file-alt"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Report Options",
        width = 350,
        selectInput(
          "report_producer",
          "Producer",
          choices = NULL
        ),
        selectInput(
          "report_year",
          "Year",
          choices = NULL
        ),
        selectInput(
          "report_depth",
          "Depth",
          choices = NULL
        ),
        radioButtons(
          "report_format",
          "Format",
          choices = c("HTML" = "html", "Word" = "docx", "All" = "all"),
          selected = "html"
        ),
        downloadButton("download_report", "Generate & Download Report")
      ),
      card(
        card_header("Soil Health Reports"),
        p(
          "Select a producer and year, then click the button to generate",
          "a soil health report. Reports include tables, strip plots,",
          "and maps comparing the producer's fields to project averages."
        ),
        p(
          "HTML reports are interactive (hover-able plots, downloadable data).",
          "Word reports are suitable for printing."
        )
      )
    )
  ),

  # Settings Tab
  nav_panel(
    title = "Settings",
    icon = icon("cog"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Theme Customization"),
        p("Theme settings coming soon...")
      ),
      card(
        card_header("API Configuration"),
        p("Configure API keys and preferences here.")
      )
    )
  ),

  # Footer
  nav_spacer(),
  nav_item(
    tags$small(
      "Data: USDA NASS | Open-Meteo API",
      style = "padding: 10px;"
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Soil Data Table
  output$soil_table <- renderDT(
    DBI::dbGetQuery(con, "SELECT * FROM soil_data"),
    options = list(pageLength = 15, scrollX = TRUE)
  )

  # Main AI Chat with Weather Tools
  chat_mod_server("main_chat", main_chat)

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
