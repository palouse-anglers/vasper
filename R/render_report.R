#' Render soil health reports
#'
#' Generates an HTML and/or DOCX soil health report for a given producer and
#' year range.
#' Data is read from database tables and written to temporary CSV files for
#' the Quarto rendering process.
#'
#' @param producer_id Character. The producer ID to filter on.
#' @param year_start Integer. Inclusive start year for the reporting window.
#' @param year_end Integer. Inclusive end year for the reporting window.
#' @param depth Character. The depth layer to filter on (e.g., "0-3", "6-12").
#'   NULL = no filter (all depths).
#' @param format Character. "html", "docx", or "all" (both formats, returned
#'   as a zip archive).
#' @param con DBI database connection. The database connection containing the
#'   soil data and dictionary tables.
#' @param data_table Character. Name of the database table containing soil data
#'   (default: "soil_data").
#' @param dictionary_table Character. Name of the database table containing the
#'   data dictionary (default: "data_dictionary").
#' @param output_dir Character. Directory where rendered output files are
#'   written. Defaults to `"."` (current working directory). The directory
#'   must already exist.
#'
#' @return The path to the rendered report file (character). When
#'   `format = "all"`, returns the path to a zip file containing both outputs.
render_report <- function(
  producer_id = REPORT_DEFAULTS$producer_id,
  year_start = 2023,
  year_end = 2023,
  depth = NULL,
  format = c("html", "docx", "all"),
  con = NULL,
  data_table = TABLE_NAMES$soil_data,
  dictionary_table = TABLE_NAMES$data_dictionary,
  output_dir = "."
) {
  format <- match.arg(format)
  year_start <- as.integer(year_start)
  year_end <- as.integer(year_end)

  if (is.na(year_start) || is.na(year_end)) {
    stop("Both 'year_start' and 'year_end' must be valid integers.")
  }

  if (year_start > year_end) {
    stop("'year_start' must be less than or equal to 'year_end'.")
  }

  year_label <- if (year_start == year_end) {
    as.character(year_start)
  } else {
    glue::glue("{year_start}-{year_end}")
  }

  # Validate database connection
  if (is.null(con) || !inherits(con, "DBIConnection")) {
    stop(
      "A valid DBI database connection must be provided via 'con' parameter."
    )
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

  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  # Export database tables to CSV files in the output directory.
  # These are cleaned up after rendering via on.exit().
  data_path <- file.path(output_dir, "soil_data.csv")
  dictionary_path <- file.path(output_dir, "data_dictionary.csv")
  on.exit(unlink(c(data_path, dictionary_path)), add = TRUE)

  soil_data <- DBI::dbReadTable(con, data_table)
  write.csv(soil_data, data_path, row.names = FALSE)

  dictionary_data <- DBI::dbReadTable(con, dictionary_table)
  write.csv(dictionary_data, dictionary_path, row.names = FALSE)

  # Build output base name (used for single-format renders)
  depth_suffix <- if (!is.null(depth)) gsub("-", "to", depth) else "AllDepths"
  base_name <- glue::glue("{year_label}_{producer_id}_{depth_suffix}_Report")

  # Common render args.
  # execute_dir is set to the template directory so that all relative paths
  # inside the QMD (images/, resources/, child docs) resolve correctly.
  # Intermediate files (figure-output/) land in template_dir but are gitignored.
  render_args <- list(
    input = file.path(template_dir, "01_producer-report.qmd"),
    execute_dir = template_dir,
    execute_params = list(
      producer_id = producer_id,
      year_start = year_start,
      year_end = year_end,
      depth = depth,
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
    rendered_files <- character()

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

      rendered_files <- c(
        rendered_files,
        glue::glue("{base_name}.{fmt}")
      )

      # Free memory from the completed render before starting the next one
      gc()
    }

    zip_path <- file.path(output_dir, glue::glue("{base_name}.zip"))

    zip::zipr(
      zip_path,
      files = rendered_files,
      root = output_dir,
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
