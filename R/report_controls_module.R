# Report Controls Module ----

#' Report controls UI
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @export
report_controls_ui <- function(id) {
  ns <- NS(id)

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
    tags$div(
      class = "mb-3",
      tags$img(
        src = "reports-images/logo.png",
        alt = "Soils logo",
        style = "height: 44px; width: auto;"
      )
    ),
    hr(),
    selectInput(
      ns("report_producer"),
      "Producer",
      choices = NULL,
      width = "100%"
    ),
    sliderInput(
      ns("report_year_range"),
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
      ns("report_depth"),
      "Depth (inches)",
      choices = NULL,
      width = "100%"
    ),
    radioButtons(
      ns("report_format"),
      "Format",
      choices = REPORT_FORMATS,
      selected = "html"
    ),
    downloadButton(ns("download_report"), "Generate & Download Report")
  )
}

#' Report controls server
#'
#' @param id Module id
#' @param con DBI connection
#' @param data_table Character table name containing soil data
#' @param dictionary_table Character table name containing dictionary data
#'
#' @return List of report parameter reactives
#' @export
report_controls_server <- function(
  id,
  con,
  data_table = TABLE_NAMES$soil_data,
  dictionary_table = TABLE_NAMES$data_dictionary
) {
  moduleServer(id, function(input, output, session) {
    observe({
      producers <- get_report_producers(con, data_table = data_table)

      updateSelectInput(
        session,
        "report_producer",
        choices = producers,
        selected = REPORT_DEFAULTS$producer_id
      )
    })

    observe({
      req(input$report_producer)

      year_bounds <- get_report_year_bounds(
        con,
        producer_id = input$report_producer,
        data_table = data_table
      )

      req(length(year_bounds) == 2)
      year_min <- year_bounds[[1]]
      year_max <- year_bounds[[2]]

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

      depths_raw <- get_report_depths(
        con,
        producer_id = input$report_producer,
        year_start = year_start,
        year_end = year_end,
        data_table = data_table
      )

      req(length(depths_raw) > 0)

      updateSelectInput(
        session,
        "report_depth",
        choices = depths_raw,
        selected = depths_raw[1]
      )
    })

    output$download_report <- downloadHandler(
      filename = function() {
        build_report_filename(
          report_format = input$report_format,
          producer_id = input$report_producer,
          report_depth = input$report_depth,
          report_year_range = input$report_year_range
        )
      },
      content = function(file) {
        req(input$report_year_range)
        req(length(input$report_year_range) == 2)

        year_start <- as.integer(min(input$report_year_range))
        year_end <- as.integer(max(input$report_year_range))

        withProgress(
          message = "Generating report",
          detail = "Preparing data",
          value = 0,
          {
            last_progress <- 0

            progress_callback <- function(value, detail = NULL) {
              bounded <- max(0, min(1, value))
              # Reserve the last 10% for local copy/download work.
              target <- min(0.9, bounded * 0.9)

              if (target <= last_progress) {
                return(invisible(NULL))
              }

              increment <- target - last_progress
              last_progress <<- target

              if (!is.null(detail) && nzchar(detail)) {
                incProgress(increment, detail = detail)
              } else {
                incProgress(increment)
              }

              invisible(NULL)
            }

            report_path <- render_report(
              producer_id = input$report_producer,
              year_start = year_start,
              year_end = year_end,
              depth = input$report_depth,
              format = input$report_format,
              con = con,
              data_table = data_table,
              dictionary_table = dictionary_table,
              output_dir = dirname(file),
              progress_callback = progress_callback
            )

            if (last_progress < 0.95) {
              incProgress(0.95 - last_progress, detail = "Preparing download")
              last_progress <- 0.95
            }

            copy_ok <- file.copy(report_path, file, overwrite = TRUE)

            if (!copy_ok) {
              stop("Failed to deliver the rendered report.")
            }

            if (last_progress < 1) {
              incProgress(1 - last_progress, detail = "Download ready")
            }
          }
        )
      }
    )

    list(
      producer_id = reactive(input$report_producer),
      year_range = reactive(input$report_year_range),
      depth = reactive(input$report_depth),
      format = reactive(input$report_format)
    )
  })
}
