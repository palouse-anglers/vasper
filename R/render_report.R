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
  output_dir = ".",
  progress_callback = NULL
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

  render_single_format <- function(
    output_format,
    output_file,
    progress_offset = 0,
    progress_scale = 1
  ) {
    report_input <- normalizePath(render_args$input, mustWork = TRUE)

    emit_progress <- function(value, detail = NULL) {
      if (is.null(progress_callback) || !is.function(progress_callback)) {
        return(invisible(NULL))
      }

      bounded <- max(0, min(1, value))
      progress_callback(bounded, detail)
      invisible(NULL)
    }

    if (!requireNamespace("processx", quietly = TRUE)) {
      do.call(
        quarto::quarto_render,
        c(
          render_args,
          list(
            output_format = output_format,
            output_file = output_file
          )
        )
      )

      emit_progress(progress_offset + progress_scale, "Report render complete")
      return(invisible(NULL))
    }

    params_file <- tempfile(fileext = ".yml")
    yaml::write_yaml(render_args$execute_params, params_file)
    on.exit(unlink(params_file), add = TRUE)

    format_label <- toupper(output_format)

    parse_step_progress <- function(line) {
      match <- regexec(
        "^\\s*([0-9]+)\\s*/\\s*([0-9]+)\\s*(?:\\[(.*)\\])?\\s*$",
        line,
        perl = TRUE
      )
      parts <- regmatches(line, match)[[1]]

      if (length(parts) == 0) {
        return(NULL)
      }

      step <- suppressWarnings(as.integer(parts[[2]]))
      total <- suppressWarnings(as.integer(parts[[3]]))
      label <- if (length(parts) >= 4) trimws(parts[[4]]) else ""

      if (is.na(step) || is.na(total) || total <= 0) {
        return(NULL)
      }

      detail <- if (nzchar(label)) {
        glue::glue("{format_label}: {label} ({step}/{total})")
      } else {
        glue::glue("{format_label}: Rendering ({step}/{total})")
      }

      list(
        value = progress_offset + progress_scale * (step / total),
        detail = detail
      )
    }

    process <- processx::process$new(
      command = quarto::quarto_path(),
      args = c(
        "render",
        basename(report_input),
        "--to",
        output_format,
        "--output",
        output_file,
        "--output-dir",
        output_dir,
        "--execute-dir",
        render_args$execute_dir,
        "--execute-params",
        params_file
      ),
      wd = render_args$execute_dir,
      stdout = "|",
      stderr = "|",
      cleanup = TRUE,
      supervise = TRUE
    )

    log_lines <- character()

    handle_lines <- function(lines) {
      if (length(lines) == 0) {
        return(invisible(NULL))
      }

      log_lines <<- c(log_lines, lines)

      for (line in lines) {
        parsed <- parse_step_progress(line)

        if (!is.null(parsed)) {
          emit_progress(parsed$value, parsed$detail)
          next
        }

        if (grepl("^\\s*processing file:", line, ignore.case = TRUE)) {
          emit_progress(
            progress_offset + progress_scale * 0.02,
            glue::glue("{format_label}: Preparing report")
          )
          next
        }

        if (grepl("^\\s*pandoc\\s*$", line, ignore.case = TRUE)) {
          emit_progress(
            progress_offset + progress_scale * 0.97,
            glue::glue("{format_label}: Running Pandoc")
          )
          next
        }

        if (grepl("^\\s*Output created:", line, ignore.case = TRUE)) {
          emit_progress(
            progress_offset + progress_scale,
            glue::glue("{format_label}: Output created")
          )
        }
      }

      invisible(NULL)
    }

    while (process$is_alive()) {
      process$poll_io(250)
      handle_lines(process$read_output_lines())
      handle_lines(process$read_error_lines())
    }

    handle_lines(process$read_output_lines())
    handle_lines(process$read_error_lines())

    exit_status <- process$get_exit_status()

    if (!identical(exit_status, 0L)) {
      stop(
        paste(
          c(
            glue::glue("Quarto render failed for format '{output_format}'."),
            tail(log_lines, 20)
          ),
          collapse = "\n"
        )
      )
    }

    emit_progress(
      progress_offset + progress_scale,
      glue::glue("{format_label}: Render complete")
    )
    invisible(NULL)
  }

  if (format == "all") {
    # Render each format separately rather than using output_format = "all".
    # Quarto's "all" mode keeps both pandoc processes and all R objects in
    # memory simultaneously, which can exceed memory limits.
    # Sequential renders let R garbage-collect between passes,
    # cutting peak memory roughly in half.
    rendered_files <- character()
    render_share <- 0.92
    per_format_share <- render_share / 2

    for (i in seq_along(c("html", "docx"))) {
      fmt <- c("html", "docx")[[i]]
      render_single_format(
        output_format = fmt,
        output_file = glue::glue("{base_name}.{fmt}"),
        progress_offset = (i - 1) * per_format_share,
        progress_scale = per_format_share
      )

      rendered_files <- c(
        rendered_files,
        glue::glue("{base_name}.{fmt}")
      )

      # Free memory from the completed render before starting the next one
      gc()
    }

    zip_path <- file.path(output_dir, glue::glue("{base_name}.zip"))

    if (!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(render_share + 0.03, "Creating ZIP archive")
    }

    zip::zipr(
      zip_path,
      files = rendered_files,
      root = output_dir,
      mode = "cherry-pick"
    )

    if (!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(1, "ZIP archive ready")
    }

    return(zip_path)
  }

  # Single format render
  output_file <- glue::glue("{base_name}.{format}")
  render_single_format(
    output_format = format,
    output_file = output_file,
    progress_offset = 0,
    progress_scale = 1
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
