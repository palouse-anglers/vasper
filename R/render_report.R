#' Render soil health reports
#'
#' Generates an HTML and/or DOCX soil health report for a given producer and year.
#' Data is read from database tables and written to temporary CSV files for
#' the Quarto rendering process.
#'
#' @param producer_id Character. The producer ID to filter on.
#' @param year Integer. The reporting year.
#' @param depth_min Numeric. Minimum depth in inches (NULL = no minimum filter).
#' @param depth_max Numeric. Maximum depth in inches (NULL = no maximum filter).
#' @param format Character. "html", "docx", or "all" (both formats, returned
#'   as a zip archive).
#' @param con DBI database connection. The database connection containing the
#'   soil data and dictionary tables.
#' @param data_table Character. Name of the database table containing soil data
#'   (default: "soil_data").
#' @param dictionary_table Character. Name of the database table containing the
#'   data dictionary (default: "data_dictionary").
#'
#' @return The path to the rendered report file (character). When
#'   `format = "all"`, returns the path to a zip file containing both outputs.
render_report <- function(
  producer_id = "COLUMBIA CONSERVATION DISTRICT",
  year = 2023,
  depth_min = NULL,
  depth_max = NULL,
  format = c("html", "docx", "all"),
  con = NULL,
  data_table = "soil_data",
  dictionary_table = "data_dictionary"
) {
  format <- match.arg(format)

  # Validate database connection
  if (is.null(con) || !inherits(con, "DBIConnection")) {
    stop("A valid DBI database connection must be provided via 'con' parameter.")
  }

  # Verify tables exist
  available_tables <- DBI::dbListTables(con)
  if (!data_table %in% available_tables) {
    stop(glue::glue("Table '{data_table}' not found in database."))
  }
  if (!dictionary_table %in% available_tables) {
    stop(glue::glue("Table '{dictionary_table}' not found in database."))
  }

  # Resolve paths
  app_root <- find_app_root()
  template_dir <- normalizePath(
    file.path(app_root, "reports"),
    mustWork = TRUE
  )

  # Create a unique temp directory for output so concurrent renders
  # don't collide.
  output_dir <- tempfile(pattern = "soil-report-")
  dir.create(output_dir)

  # Export database tables to CSV files in the output directory
  data_path <- file.path(output_dir, "soil_data.csv")
  dictionary_path <- file.path(output_dir, "data_dictionary.csv")

  soil_data <- DBI::dbReadTable(con, data_table)
  write.csv(soil_data, data_path, row.names = FALSE)

  dictionary_data <- DBI::dbReadTable(con, dictionary_table)
  write.csv(dictionary_data, dictionary_path, row.names = FALSE)

  # Build output base name (used for single-format renders)
  base_name <- glue::glue("{year}_{producer_id}_Report")

  # Common render args.
  # execute_dir is set to the template directory so that all relative paths
  # inside the QMD (images/, resources/, child docs) resolve correctly.
  # Intermediate files (figure-output/) land in template_dir but are gitignored.
  render_args <- list(
    input = file.path(template_dir, "01_producer-report.qmd"),
    execute_dir = template_dir,
    execute_params = list(
      producer_id = producer_id,
      year = year,
      depth_min = depth_min,
      depth_max = depth_max,
      data_path = data_path,
      dictionary_path = dictionary_path
    ),
    quarto_args = c("--output-dir", output_dir)
  )

  if (format == "all") {
    # Render each format separately rather than using output_format = "all".
    # Quarto's "all" mode keeps both pandoc processes and all R objects in
    # memory simultaneously, which can exceed memory limits.
    # Sequential renders let R garbage-collect between passes,
    # cutting peak memory roughly in half.
    for (fmt in c("html", "docx")) {
      do.call(
        quarto::quarto_render,
        c(
          render_args,
          list(
            output_format = fmt,
            output_file = glue::glue("{base_name}.{fmt}")
          )
        )
      )
      # Free memory from the completed render before starting the next one
      gc()
    }

    rendered <- list.files(output_dir, full.names = TRUE)
    zip_path <- file.path(output_dir, glue::glue("{base_name}.zip"))
    zip::zipr(
      zip_path,
      files = rendered,
      mode = "cherry-pick"
    )
    return(zip_path)
  }

  # Single format render
  output_file <- glue::glue("{base_name}.{format}")
  do.call(
    quarto::quarto_render,
    c(
      render_args,
      list(
        output_format = format,
        output_file = output_file
      )
    )
  )

  file.path(output_dir, output_file)
}


#' Find the application root directory
#'
#' Works whether called from Shiny (app.R location), interactively, or via
#' `here::here()`. Falls back to the directory containing app.R.
#' @noRd
find_app_root <- function() {
  # If here is available and finds the project, use it
  if (requireNamespace("here", quietly = TRUE)) {
    root <- tryCatch(here::here(), error = function(e) NULL)
    if (!is.null(root) && file.exists(file.path(root, "app.R"))) {
      return(root)
    }
  }

  # In a Shiny context, the app directory is the working directory at startup

  # which is typically where app.R lives
  wd <- getwd()
  if (file.exists(file.path(wd, "app.R"))) {
    return(wd)
  }

  stop(
    "Cannot determine application root directory. ",
    "Ensure the working directory contains app.R or that the 'here' package ",
    "can find the project root."
  )
}
