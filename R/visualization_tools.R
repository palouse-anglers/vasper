# Visualization Tool Helpers ----

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Ensure visualization artifact metadata registry exists
#'
#' @param con DBI connection
#'
#' @return Invisibly TRUE
#' @export
ensure_visual_artifact_metadata <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS visual_artifact_metadata (
      artifact_id VARCHAR PRIMARY KEY,
      artifact_type VARCHAR NOT NULL,
      table_name VARCHAR NOT NULL,
      table_label VARCHAR,
      artifact_label VARCHAR,
      title VARCHAR,
      subtitle VARCHAR,
      description VARCHAR,
      spec_json VARCHAR,
      svg_path VARCHAR,
      png_path VARCHAR,
      source VARCHAR,
      source_detail VARCHAR,
      is_active INTEGER DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
    "
  )

  invisible(TRUE)
}

# -- Normalizer utilities -----------------------------------------------------

normalize_viz_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  value <- as.character(x[[1]])
  value <- trimws(value)

  if (!nzchar(value)) {
    return(default)
  }

  value
}

normalize_viz_identifier <- function(x, default_prefix = "artifact") {
  x <- normalize_viz_scalar(x, default = NA_character_)

  if (is.na(x)) {
    x <- paste0(default_prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }

  x <- tolower(x)
  x <- gsub("[^a-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  if (!nzchar(x)) {
    x <- paste0(default_prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }

  x
}

normalize_viz_dimension <- function(x, default_value) {
  if (is.null(x) || length(x) == 0) {
    return(default_value)
  }

  value <- suppressWarnings(as.numeric(x[[1]]))

  if (is.na(value) || value <= 0) {
    return(default_value)
  }

  value
}

normalize_viz_table_names <- function(table_names) {
  if (is.null(table_names)) {
    return(character())
  }

  values <- unlist(table_names, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- trimws(values)
  values <- values[nzchar(values)]

  unique(values)
}

plot_column_map_type <- function(
  description = "Named mapping of schema parameters to column names or values."
) {
  ellmer::type_object(
    .description = description,
    x = ellmer::type_string("X-axis column name."),
    y = ellmer::type_string("Y-axis column name."),
    geom = ellmer::type_string(
      "Geometry for basic schema (point/line/col/boxplot/histogram/density)."
    ),
    color = ellmer::type_string("Optional grouping/color column name."),
    facet = ellmer::type_string("Optional facet column name."),
    fill = ellmer::type_string("Optional fill column name."),
    metrics = ellmer::type_array(
      ellmer::type_string(),
      "Metric column names for multi-metric schemas."
    ),
    y1 = ellmer::type_string("Primary y column for dual-axis schema."),
    y2 = ellmer::type_string("Secondary y column for dual-axis schema."),
    threshold = ellmer::type_number("Optional threshold reference value."),
    .additional_properties = TRUE
  )
}

normalize_viz_named_list <- function(x, param_name = "value") {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }

  parse_named_json_object <- function(value) {
    if (!is.character(value) || length(value) != 1) {
      return(NULL)
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(value[[1]], simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (!is.list(parsed) || is.null(names(parsed))) {
      return(NULL)
    }

    parsed
  }

  parse_named_pair_array <- function(value) {
    if (!is.list(value) || length(value) == 0) {
      return(NULL)
    }

    if (!all(vapply(value, is.list, logical(1)))) {
      return(NULL)
    }

    keys <- vapply(
      value,
      function(item) {
        key <- item$name %||% item$key %||% item$field %||% ""
        if (!is.character(key) || length(key) == 0) {
          return("")
        }
        trimws(as.character(key[[1]]))
      },
      character(1)
    )

    if (!all(nzchar(keys))) {
      return(NULL)
    }

    values <- lapply(value, function(item) {
      item$value %||% item$val %||% item$data %||% item[[2]] %||% NULL
    })

    stats::setNames(values, keys)
  }

  out <- NULL

  if (is.list(x) && !is.null(names(x)) && any(nzchar(trimws(names(x))))) {
    out <- x
  } else if (
    is.list(x) &&
      length(x) == 1 &&
      is.list(x[[1]]) &&
      !is.null(names(x[[1]])) &&
      any(nzchar(trimws(names(x[[1]]))))
  ) {
    out <- x[[1]]
  } else if (is.character(x) && length(x) == 1) {
    out <- parse_named_json_object(x)
  } else if (
    is.list(x) && length(x) == 1 && is.character(x[[1]]) && length(x[[1]]) == 1
  ) {
    out <- parse_named_json_object(x[[1]])
  }

  if (is.null(out)) {
    out <- parse_named_pair_array(x)
  }

  if (is.null(out)) {
    stop(
      paste0(
        "'",
        param_name,
        "' must be a named object/list (or JSON object string)."
      ),
      call. = FALSE
    )
  }

  # Unwrap common payload wrappers from tool invocations
  wrapper_keys <- unique(c(
    param_name,
    "value",
    "data",
    "mapping",
    "column_map"
  ))
  repeat {
    nm <- names(out) %||% character()

    if (length(out) != 1 || length(nm) != 1 || !nzchar(nm[[1]])) {
      break
    }

    key <- trimws(nm[[1]])
    inner <- out[[1]]

    if (is.character(inner) && length(inner) == 1) {
      parsed_inner <- parse_named_json_object(inner)
      if (!is.null(parsed_inner)) {
        out <- parsed_inner
        next
      }

      pair_array <- tryCatch(
        jsonlite::fromJSON(inner[[1]], simplifyVector = FALSE),
        error = function(e) NULL
      )
      parsed_pairs <- parse_named_pair_array(pair_array)
      if (!is.null(parsed_pairs)) {
        out <- parsed_pairs
        next
      }
    }

    if (key %in% wrapper_keys && is.list(inner) && !is.null(names(inner))) {
      out <- inner
      next
    }

    break
  }

  nm <- names(out) %||% character()
  nm <- tolower(trimws(nm))
  names(out) <- nm
  out <- out[nzchar(names(out))]
  out <- out[!duplicated(names(out), fromLast = TRUE)]

  is_effective_value <- function(v) {
    if (is.null(v) || length(v) == 0) {
      return(FALSE)
    }

    if (all(is.na(v))) {
      return(FALSE)
    }

    if (is.character(v)) {
      vv <- trimws(v[!is.na(v)])
      return(length(vv) > 0 && any(nzchar(vv)))
    }

    TRUE
  }

  out <- out[vapply(out, is_effective_value, logical(1))]

  out
}

assert_table_and_columns <- function(con, table_name, columns) {
  if (!DBI::dbExistsTable(con, table_name)) {
    stop(paste0("Table not found: ", table_name, "."), call. = FALSE)
  }

  table_columns <- DBI::dbListFields(con, table_name)
  missing <- setdiff(unique(columns[!is.na(columns)]), table_columns)

  if (length(missing) > 0) {
    stop(
      paste0(
        "Column(s) not found in table '",
        table_name,
        "': ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(table_columns)
}

# -- Data readers -------------------------------------------------------------

read_plot_data <- function(con, table_name, columns, limit_rows = 50000L) {
  quoted_table <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  quoted_cols <- vapply(
    columns,
    function(col) as.character(DBI::dbQuoteIdentifier(con, col)),
    character(1)
  )

  sql <- paste0(
    "SELECT ",
    paste(quoted_cols, collapse = ", "),
    " FROM ",
    quoted_table,
    " LIMIT ",
    as.integer(limit_rows)
  )

  DBI::dbGetQuery(con, sql)
}

read_plot_code_table <- function(con, table_name, limit_rows = 50000L) {
  quoted_table <- as.character(DBI::dbQuoteIdentifier(con, table_name))

  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      quoted_table,
      " LIMIT ",
      as.integer(limit_rows)
    )
  )
}

# -- Plot file I/O ------------------------------------------------------------

get_plot_artifact_base_dir <- function(kind = "plot") {
  root_dir <- getOption(
    "vasper.artifact_dir",
    file.path(tempdir(), "vasper-artifacts")
  )

  base_dir <- file.path(root_dir, kind)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  normalizePath(base_dir, winslash = "/", mustWork = FALSE)
}

build_plot_artifact_paths <- function(artifact_id, kind = "plot") {
  base_dir <- get_plot_artifact_base_dir(kind)

  svg_path <- file.path(base_dir, paste0(artifact_id, ".svg"))
  png_path <- file.path(base_dir, paste0(artifact_id, ".png"))

  list(svg_path = svg_path, png_path = png_path)
}

run_plot_code_isolated <- function(
  plot_code,
  table_data_list,
  files,
  width,
  height,
  dpi,
  title = NA_character_,
  subtitle = NA_character_
) {
  title <- normalize_viz_scalar(title, default = NA_character_)
  subtitle <- normalize_viz_scalar(subtitle, default = NA_character_)

  callr::r(
    function(
      plot_code,
      table_data_list,
      files,
      width,
      height,
      dpi,
      title,
      subtitle,
      brand_colors,
      map_palette_green,
      map_palette_categorical,
      plot_palette_categorical,
      plot_palette_continuous,
      plot_color_na
    ) {
      suppressPackageStartupMessages({
        library(ggplot2)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(forcats)
        library(lubridate)
        library(scales)
        library(patchwork)
        library(ggridges)
      })

      eval_env <- new.env(parent = globalenv())

      eval_env$ggplot2 <- asNamespace("ggplot2")
      eval_env$dplyr <- asNamespace("dplyr")
      eval_env$tidyr <- asNamespace("tidyr")
      eval_env$stringr <- asNamespace("stringr")
      eval_env$forcats <- asNamespace("forcats")
      eval_env$lubridate <- asNamespace("lubridate")
      eval_env$scales <- asNamespace("scales")
      eval_env$patchwork <- asNamespace("patchwork")
      eval_env$ggridges <- asNamespace("ggridges")

      eval_env$BRAND_COLORS <- brand_colors
      eval_env$MAP_PALETTE_GREEN <- map_palette_green
      eval_env$MAP_PALETTE_CATEGORICAL <- map_palette_categorical
      eval_env$PLOT_PALETTE_CATEGORICAL <- plot_palette_categorical
      eval_env$PLOT_PALETTE_CONTINUOUS <- plot_palette_continuous
      eval_env$PLOT_COLOR_NA <- plot_color_na

      plot_palette_discrete <- function(x = NULL, n = NULL) {
        if (is.null(n)) {
          n <- if (is.null(x)) {
            length(plot_palette_categorical)
          } else {
            length(unique(as.character(stats::na.omit(x))))
          }
        }

        n <- as.integer(n)
        if (is.na(n) || n <= 0) {
          return(character())
        }

        if (n <= length(plot_palette_categorical)) {
          palette <- plot_palette_categorical[seq_len(n)]
        } else {
          palette <- grDevices::colorRampPalette(plot_palette_categorical)(n)
        }

        if (!is.null(x)) {
          keys <- unique(as.character(stats::na.omit(x)))
          keys <- sort(keys)
          names(palette) <- keys
        }

        palette
      }

      scale_colour_vasper_discrete <- function(x = NULL, n = NULL, ...) {
        ggplot2::scale_colour_manual(
          values = plot_palette_discrete(x = x, n = n),
          na.value = plot_color_na,
          ...
        )
      }

      scale_fill_vasper_discrete <- function(x = NULL, n = NULL, ...) {
        ggplot2::scale_fill_manual(
          values = plot_palette_discrete(x = x, n = n),
          na.value = plot_color_na,
          ...
        )
      }

      eval_env$plot_palette_discrete <- plot_palette_discrete
      eval_env$scale_colour_vasper_discrete <- scale_colour_vasper_discrete
      eval_env$scale_fill_vasper_discrete <- scale_fill_vasper_discrete

      eval_env$tables <- table_data_list
      for (nm in names(table_data_list)) {
        assign(nm, table_data_list[[nm]], envir = eval_env)
      }

      expr <- parse(text = plot_code)
      plot_obj <- eval(expr, envir = eval_env)

      if (!inherits(plot_obj, "ggplot") && !inherits(plot_obj, "patchwork")) {
        stop(
          "plot_code must evaluate to a ggplot or patchwork object.",
          call. = FALSE
        )
      }

      if (!is.na(title) || !is.na(subtitle)) {
        plot_obj <- plot_obj +
          ggplot2::labs(
            title = if (is.na(title)) NULL else title,
            subtitle = if (is.na(subtitle)) NULL else subtitle
          )
      }

      dir.create(
        dirname(files$svg_path),
        recursive = TRUE,
        showWarnings = FALSE
      )
      dir.create(
        dirname(files$png_path),
        recursive = TRUE,
        showWarnings = FALSE
      )

      ggplot2::ggsave(
        filename = files$svg_path,
        plot = plot_obj,
        device = grDevices::svg,
        width = width,
        height = height,
        units = "in"
      )

      ggplot2::ggsave(
        filename = files$png_path,
        plot = plot_obj,
        dpi = as.integer(dpi),
        width = width,
        height = height,
        units = "in"
      )

      list(
        svg_path = files$svg_path,
        png_path = files$png_path
      )
    },
    args = list(
      plot_code = plot_code,
      table_data_list = table_data_list,
      files = files,
      width = width,
      height = height,
      dpi = as.integer(dpi),
      title = title,
      subtitle = subtitle,
      brand_colors = BRAND_COLORS,
      map_palette_green = MAP_PALETTE_GREEN,
      map_palette_categorical = MAP_PALETTE_CATEGORICAL,
      plot_palette_categorical = PLOT_PALETTE_CATEGORICAL,
      plot_palette_continuous = PLOT_PALETTE_CONTINUOUS,
      plot_color_na = PLOT_COLOR_NA
    )
  )
}

# -- Metadata persistence -----------------------------------------------------

upsert_visual_artifact_metadata <- function(
  con,
  artifact_id,
  artifact_type,
  table_name,
  table_label,
  artifact_label,
  title,
  subtitle,
  description = NA_character_,
  spec,
  svg_path,
  png_path,
  source = "tool",
  source_detail = NA_character_,
  is_active = TRUE
) {
  ensure_visual_artifact_metadata(con)

  DBI::dbExecute(
    con,
    "DELETE FROM visual_artifact_metadata WHERE artifact_id = ?",
    params = list(artifact_id)
  )

  DBI::dbExecute(
    con,
    "
    INSERT INTO visual_artifact_metadata (
      artifact_id,
      artifact_type,
      table_name,
      table_label,
      artifact_label,
      title,
      subtitle,
      description,
      spec_json,
      svg_path,
      png_path,
      source,
      source_detail,
      is_active,
      created_at,
      updated_at
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
    ",
    params = list(
      artifact_id,
      artifact_type,
      table_name,
      table_label,
      artifact_label,
      title,
      subtitle,
      description,
      jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"),
      svg_path,
      png_path,
      source,
      source_detail,
      as.integer(is_active)
    )
  )

  invisible(TRUE)
}

# -- Schema-based plot creation -----------------------------------------------

#' Resolve basic schema geom layer string
resolve_basic_geom_layer <- function(geom) {
  switch(
    geom,
    point = "geom_point(alpha = 0.8, size = 2)",
    line = "geom_line(alpha = 0.9, linewidth = 0.8)",
    col = "geom_col(alpha = 0.9)",
    boxplot = "geom_boxplot(alpha = 0.8, outlier.alpha = 0.2)",
    histogram = "geom_histogram(bins = 30, alpha = 0.9)",
    density = "geom_density(alpha = 0.35)",
    stop(paste0("Unsupported geom: ", geom), call. = FALSE)
  )
}

#' Build plot code from a schema and column mappings
#'
#' @param schema A plot schema list from PLOT_SCHEMAS
#' @param column_map Named list mapping parameter names to values
#' @param overrides Named list of optional overrides (title, subtitle)
#' @return R code string ready for isolated plot evaluation
build_code_from_schema <- function(schema, column_map, overrides = list()) {
  template <- schema$template

  # For basic schema, resolve the geom layer
  if (identical(schema$name, "basic")) {
    geom <- column_map$geom %||% "point"
    geom_layer <- resolve_basic_geom_layer(geom)
    column_map$geom_layer <- geom_layer
  }

  # For multi_metric_facet_bar, build quoted metrics vector
  if (
    identical(schema$name, "multi_metric_facet_bar") &&
      !is.null(column_map$metrics)
  ) {
    metrics <- unlist(column_map$metrics, use.names = FALSE)
    quoted <- paste0('"', metrics, '"', collapse = ", ")
    column_map$metrics_quoted <- quoted
  }

  # Build full replacement values
  all_values <- c(column_map, overrides)

  # Expand conditional blocks: {{#key}}...{{/key}}
  for (nm in names(all_values)) {
    val <- as.character(all_values[[nm]] %||% "")
    open_tag <- paste0("{{#", nm, "}}")

    if (grepl(open_tag, template, fixed = TRUE)) {
      pattern <- paste0(
        "\\{\\{#",
        nm,
        "\\}\\}",
        "(?s)(.*?)",
        "\\{\\{/",
        nm,
        "\\}\\}"
      )

      if (nzchar(val) && !is.na(val)) {
        template <- gsub(pattern, "\\1", template, perl = TRUE)
      } else {
        template <- gsub(pattern, "", template, perl = TRUE)
      }
    }
  }

  # Remove any leftover conditional blocks whose keys were not provided
  template <- gsub(
    "\\{\\{#[^}]+\\}\\}(?s)(.*?)\\{\\{/[^}]+\\}\\}",
    "",
    template,
    perl = TRUE
  )

  # Substitute simple {{key}} placeholders
  template <- substitute_template(template, all_values)

  # Replace any remaining unresolved {{key}} placeholders (optional params not
  # supplied) with empty string so nzchar() guards evaluate correctly at runtime
  template <- gsub("\\{\\{[^}]+\\}\\}", "", template)

  # Apply title/subtitle if provided — append labs() to the final expression
  title_val <- overrides$title %||% ""
  subtitle_val <- overrides$subtitle %||% ""

  if (nzchar(title_val) || nzchar(subtitle_val)) {
    parts <- c()
    if (nzchar(title_val)) {
      parts <- c(parts, paste0('title = "', title_val, '"'))
    }
    if (nzchar(subtitle_val)) {
      parts <- c(parts, paste0('subtitle = "', subtitle_val, '"'))
    }
    template <- paste0(
      trimws(template, which = "right"),
      " +\n",
      "  ggplot2::labs(",
      paste(parts, collapse = ", "),
      ")"
    )
  }

  template
}

#' Create a plot artifact from a named schema
run_create_plot_from_schema <- function(
  con,
  schema_name,
  table_name,
  column_map,
  description,
  title = NULL,
  subtitle = NULL,
  artifact_name,
  artifact_label = NULL,
  width = 9,
  height = 5,
  dpi = 180,
  add_data_view = TRUE
) {
  schema_name <- normalize_viz_scalar(schema_name)
  table_name <- normalize_viz_scalar(table_name)
  column_map <- normalize_viz_named_list(column_map, param_name = "column_map")
  description <- normalize_viz_scalar(description)
  title <- normalize_viz_scalar(title, default = NA_character_)
  subtitle <- normalize_viz_scalar(subtitle, default = NA_character_)

  if (is.na(schema_name)) {
    stop("'schema_name' is required.", call. = FALSE)
  }
  if (is.na(table_name)) {
    stop("'table_name' is required.", call. = FALSE)
  }
  if (is.na(description)) {
    stop("'description' is required.", call. = FALSE)
  }
  if (is.null(artifact_name) || length(artifact_name) == 0) {
    stop(
      "'artifact_name' is required for create_plot_from_schema.",
      call. = FALSE
    )
  }

  artifact_name <- normalize_viz_scalar(artifact_name)
  if (is.na(artifact_name)) {
    stop(
      "'artifact_name' is required for create_plot_from_schema.",
      call. = FALSE
    )
  }

  schema <- get_plot_schema(schema_name)
  if (is.null(schema)) {
    available <- paste(list_plot_schema_names(), collapse = ", ")
    stop(
      paste0("Unknown schema '", schema_name, "'. Available: ", available, "."),
      call. = FALSE
    )
  }

  # Validate required parameters
  required_params <- Filter(
    function(p) isTRUE(p$required),
    schema$parameters
  )
  required_names <- vapply(required_params, function(p) p$name, character(1))
  missing_params <- setdiff(required_names, names(column_map))

  if (length(missing_params) > 0) {
    received_keys <- names(column_map) %||% character()
    received_txt <- if (length(received_keys) == 0) {
      "(none)"
    } else {
      paste(received_keys, collapse = ", ")
    }

    stop(
      paste0(
        "Missing required parameter(s) for schema '",
        schema_name,
        "': ",
        paste(missing_params, collapse = ", "),
        ". Received keys: ",
        received_txt,
        "."
      ),
      call. = FALSE
    )
  }

  # Collect column names for DB validation
  col_names <- character()
  for (nm in names(column_map)) {
    val <- column_map[[nm]]
    if (is.character(val) && length(val) >= 1) {
      param_spec <- Filter(function(p) p$name == nm, schema$parameters)
      if (
        length(param_spec) > 0 &&
          param_spec[[1]]$type %in% c("column", "column_list")
      ) {
        col_names <- c(col_names, unlist(val))
      }
    }
  }

  col_names <- unique(col_names[nzchar(col_names)])
  if (length(col_names) > 0) {
    assert_table_and_columns(con, table_name, col_names)
  } else if (!DBI::dbExistsTable(con, table_name)) {
    stop(paste0("Table not found: ", table_name, "."), call. = FALSE)
  }

  artifact_id <- normalize_viz_identifier(
    artifact_name,
    default_prefix = "plot"
  )
  artifact_label_val <- normalize_viz_scalar(
    artifact_label,
    default = artifact_id
  )

  size_w <- normalize_viz_dimension(width, 9)
  size_h <- normalize_viz_dimension(height, 5)
  size_dpi <- as.integer(normalize_viz_dimension(dpi, 180))

  # Build code from schema template
  overrides <- list()
  if (!is.na(title)) {
    overrides$title <- title
  }
  if (!is.na(subtitle)) {
    overrides$subtitle <- subtitle
  }

  plot_code <- build_code_from_schema(schema, column_map, overrides)

  # Load data and evaluate
  plot_df <- read_plot_code_table(con, table_name, limit_rows = 50000L)

  files <- build_plot_artifact_paths(artifact_id = artifact_id, kind = "plots")

  files <- tryCatch(
    run_plot_code_isolated(
      plot_code = plot_code,
      table_data_list = list(data = plot_df),
      files = files,
      width = size_w,
      height = size_h,
      dpi = size_dpi
    ),
    error = function(e) {
      stop(
        paste0(
          "Schema '",
          schema_name,
          "' template error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  spec <- list(
    artifact_type = "plot",
    mode = "schema",
    schema_name = schema_name,
    table_name = table_name,
    column_map = column_map,
    description = description,
    title = if (is.na(title)) NULL else title,
    subtitle = if (is.na(subtitle)) NULL else subtitle,
    width = size_w,
    height = size_h,
    dpi = size_dpi,
    plot_code = plot_code
  )

  upsert_visual_artifact_metadata(
    con = con,
    artifact_id = artifact_id,
    artifact_type = "plot",
    table_name = table_name,
    table_label = table_name,
    artifact_label = artifact_label_val,
    title = title,
    subtitle = subtitle,
    description = description,
    spec = spec,
    svg_path = files$svg_path,
    png_path = files$png_path,
    source = "create_plot_from_schema",
    source_detail = paste0(
      "tool=create_plot_from_schema; schema=",
      schema_name,
      "; table_name=",
      table_name
    ),
    is_active = TRUE
  )

  list(
    artifact_id = artifact_id,
    artifact_type = "plot",
    artifact_label = artifact_label_val,
    table_name = TABLE_NAMES$visual_artifact_metadata,
    table_label = "Visualization Artifacts",
    add_data_view = isTRUE(add_data_view),
    spec = spec,
    files = list(svg = files$svg_path, png = files$png_path)
  )
}

# -- Read schemas tool handler ------------------------------------------------

run_list_plot_schemas <- function() {
  catalog <- get_plot_schema_catalog()

  lapply(catalog, function(s) {
    list(
      name = s$name,
      description = s$description,
      parameters = s$parameters
    )
  })
}

run_read_plot_schemas <- function(schema_names) {
  schema_names <- normalize_viz_table_names(schema_names)

  if (length(schema_names) == 0) {
    stop(
      "'schema_names' must include at least one schema name.",
      call. = FALSE
    )
  }

  available <- list_plot_schema_names()
  unknown <- setdiff(schema_names, available)
  if (length(unknown) > 0) {
    stop(
      paste0(
        "Unknown schema name(s): ",
        paste(unknown, collapse = ", "),
        ". Available: ",
        paste(available, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  catalog <- get_plot_schema_catalog()[schema_names]

  lapply(catalog, function(s) {
    list(
      name = s$name,
      description = s$description,
      parameters = s$parameters,
      template = s$template
    )
  })
}

# -- Code-based plot creation (with inspiration gate) -------------------------

run_create_plot_code <- function(
  con,
  plot_code,
  table_names,
  description,
  inspiration_schemas = character(),
  artifact_name,
  artifact_label = NULL,
  title = NULL,
  subtitle = NULL,
  width = 9,
  height = 5,
  dpi = 180,
  limit_rows = 50000L,
  add_data_view = TRUE
) {
  plot_code <- normalize_viz_scalar(plot_code)
  table_names <- normalize_viz_table_names(table_names)
  description <- normalize_viz_scalar(description)

  if (is.na(plot_code)) {
    stop("'plot_code' is required.", call. = FALSE)
  }
  if (length(table_names) == 0) {
    stop("'table_names' must include at least one table.", call. = FALSE)
  }
  if (is.na(description)) {
    stop("'description' is required for create_plot_code.", call. = FALSE)
  }
  if (is.null(artifact_name) || length(artifact_name) == 0) {
    stop("'artifact_name' is required for create_plot_code.", call. = FALSE)
  }

  artifact_name <- normalize_viz_scalar(artifact_name)
  if (is.na(artifact_name)) {
    stop("'artifact_name' is required for create_plot_code.", call. = FALSE)
  }

  # Validate inspiration schemas
  inspiration_schemas <- normalize_viz_table_names(inspiration_schemas)
  if (length(inspiration_schemas) == 0) {
    stop(
      paste0(
        "'inspiration_schemas' is required. Call list_plot_schemas/read_plot_schemas first, ",
        "then list one or more schema names that inspired your code."
      ),
      call. = FALSE
    )
  }

  available_schemas <- list_plot_schema_names()
  unknown_schemas <- setdiff(inspiration_schemas, available_schemas)
  if (length(unknown_schemas) > 0) {
    stop(
      paste0(
        "Unknown inspiration schema(s): ",
        paste(unknown_schemas, collapse = ", "),
        ". Available: ",
        paste(available_schemas, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  for (tbl in table_names) {
    if (!DBI::dbExistsTable(con, tbl)) {
      stop(paste0("Table not found: ", tbl, "."), call. = FALSE)
    }
  }

  artifact_id <- normalize_viz_identifier(
    artifact_name,
    default_prefix = "plot"
  )
  artifact_label <- normalize_viz_scalar(
    artifact_label,
    default = artifact_id
  )
  title <- normalize_viz_scalar(title, default = NA_character_)
  subtitle <- normalize_viz_scalar(subtitle, default = NA_character_)

  size_w <- normalize_viz_dimension(width, 9)
  size_h <- normalize_viz_dimension(height, 5)
  size_dpi <- as.integer(normalize_viz_dimension(dpi, 180))
  limit_rows <- as.integer(normalize_viz_dimension(limit_rows, 50000L))

  # Build inspiration header from schema templates
  inspiration_header <- paste(
    vapply(
      inspiration_schemas,
      function(sn) {
        schema <- get_plot_schema(sn)
        paste0(
          "# --- Inspiration schema: ",
          sn,
          " ---\n",
          "# ",
          gsub("\n", "\n# ", schema$description),
          "\n",
          "# Template code:\n",
          paste0("# ", strsplit(schema$template, "\n")[[1]], collapse = "\n"),
          "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )

  full_code <- paste0(inspiration_header, "\n", plot_code)

  table_data <- lapply(table_names, function(tbl) {
    read_plot_code_table(con, tbl, limit_rows = limit_rows)
  })
  names(table_data) <- table_names

  files <- build_plot_artifact_paths(artifact_id = artifact_id, kind = "plots")

  files <- tryCatch(
    run_plot_code_isolated(
      plot_code = full_code,
      table_data_list = table_data,
      files = files,
      width = size_w,
      height = size_h,
      dpi = size_dpi,
      title = title,
      subtitle = subtitle
    ),
    error = function(e) {
      stop(
        paste0("Error evaluating plot_code: ", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  # Optional sub-LLM review (advisory only; no auto-fix)
  plot_review <- review_plot(
    plot_code = plot_code,
    description = description,
    png_path = files$png_path
  )

  spec <- list(
    artifact_type = "plot",
    mode = "code",
    table_names = table_names,
    plot_code = plot_code,
    description = description,
    inspiration_schemas = inspiration_schemas,
    title = if (is.na(title)) NULL else title,
    subtitle = if (is.na(subtitle)) NULL else subtitle,
    width = size_w,
    height = size_h,
    dpi = size_dpi,
    limit_rows = limit_rows
  )

  source_table <- table_names[[1]]
  source_label <- if (length(table_names) == 1) {
    source_table
  } else {
    paste0(source_table, " (+", length(table_names) - 1, " more)")
  }

  upsert_visual_artifact_metadata(
    con = con,
    artifact_id = artifact_id,
    artifact_type = "plot",
    table_name = source_table,
    table_label = source_label,
    artifact_label = artifact_label,
    title = title,
    subtitle = subtitle,
    description = description,
    spec = spec,
    svg_path = files$svg_path,
    png_path = files$png_path,
    source = "create_plot_code",
    source_detail = paste0(
      "tool=create_plot_code; tables=",
      paste(table_names, collapse = ","),
      "; code_hash=",
      digest::digest(plot_code, algo = "md5")
    ),
    is_active = TRUE
  )

  list(
    artifact_id = artifact_id,
    artifact_type = "plot",
    artifact_label = artifact_label,
    table_name = TABLE_NAMES$visual_artifact_metadata,
    table_label = "Visualization Artifacts",
    add_data_view = isTRUE(add_data_view),
    spec = spec,
    plot_review = plot_review,
    files = list(svg = files$svg_path, png = files$png_path)
  )
}

# -- Sub-LLM plot review ------------------------------------------------------

#' Review a rendered plot using a vision-capable sub-LLM
#'
#' Controlled by env vars VASPER_PLOT_REFINEMENT_ENABLED and
#' VASPER_PLOT_REFINEMENT_MODEL. Gracefully skips when disabled or on error.
#' Returns advisory feedback only; the main LLM decides whether to revise code.
review_plot <- function(
  plot_code,
  description,
  png_path
) {
  enabled <- tolower(trimws(Sys.getenv("VASPER_PLOT_REFINEMENT_ENABLED", "")))
  if (!enabled %in% c("true", "1", "yes")) {
    return(list(
      enabled = FALSE,
      ok = NA,
      suggestion = "",
      model = NULL
    ))
  }

  model <- Sys.getenv("VASPER_PLOT_REFINEMENT_MODEL", "gpt-4o-mini")

  png_path <- normalize_viz_scalar(png_path, default = NA_character_)
  if (is.na(png_path) || !file.exists(png_path)) {
    return(list(
      enabled = TRUE,
      ok = NA,
      suggestion = "",
      model = model,
      error = "Rendered PNG not found for plot refinement."
    ))
  }

  refinement_prompt <- paste0(
    "You are a data visualization refinement checker.\n\n",
    "Description of what this plot should show:\n",
    description,
    "\n\n",
    "The R code used:\n```r\n",
    plot_code,
    "\n```\n\n",
    "Does this plot correctly represent the description? ",
    "Reply ONLY with valid JSON: {\"ok\": true/false, \"fix_hint\": \"string\"}.\n",
    "If ok is true, fix_hint should be empty string. ",
    "If ok is false, fix_hint should be a concise instruction for fixing the code."
  )

  result <- tryCatch(
    {
      chat <- ellmer::chat_openai(
        model = model,
        system_prompt = paste(
          "You refine data visualizations.",
          "Reply only with JSON."
        )
      )
      response <- chat$chat(
        ellmer::content_image_file(png_path),
        refinement_prompt
      )
      jsonlite::fromJSON(response, simplifyVector = FALSE)
    },
    error = function(e) {
      list(ok = NA, fix_hint = "", error = conditionMessage(e))
    }
  )

  list(
    enabled = TRUE,
    ok = isTRUE(result$ok),
    suggestion = result$fix_hint %||% "",
    model = model,
    error = result$error %||% NULL
  )
}

# -- Metadata query -----------------------------------------------------------

get_visual_artifact_metadata <- function(
  con,
  artifact_ids = NULL,
  artifact_type = NULL,
  include_inactive = FALSE
) {
  ensure_visual_artifact_metadata(con)

  where_clauses <- c()
  params <- list()

  if (!isTRUE(include_inactive)) {
    where_clauses <- c(where_clauses, "is_active = 1")
  }

  if (!is.null(artifact_ids) && length(artifact_ids) > 0) {
    ids <- unlist(artifact_ids, recursive = TRUE, use.names = FALSE)
    ids <- as.character(ids)
    ids <- trimws(ids)
    ids <- ids[nzchar(ids)]

    if (length(ids) > 0) {
      placeholders <- paste(rep("?", length(ids)), collapse = ",")
      where_clauses <- c(
        where_clauses,
        paste0("artifact_id IN (", placeholders, ")")
      )
      params <- c(params, as.list(ids))
    }
  }

  if (!is.null(artifact_type) && length(artifact_type) > 0) {
    typ <- tolower(normalize_viz_scalar(artifact_type, default = ""))
    if (nzchar(typ)) {
      where_clauses <- c(where_clauses, "LOWER(artifact_type) = ?")
      params <- c(params, list(typ))
    }
  }

  sql <- "
    SELECT
      artifact_id,
      artifact_type,
      table_name,
      table_label,
      artifact_label,
      title,
      subtitle,
      description,
      spec_json,
      svg_path,
      png_path,
      source,
      source_detail,
      is_active,
      updated_at
    FROM visual_artifact_metadata
  "

  if (length(where_clauses) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where_clauses, collapse = " AND "))
  }

  sql <- paste0(sql, " ORDER BY updated_at DESC")

  out <- DBI::dbGetQuery(con, sql, params = params)

  if (nrow(out) == 0) {
    return(list(artifacts = list()))
  }

  out$spec <- lapply(out$spec_json, function(x) {
    tryCatch(
      jsonlite::fromJSON(x, simplifyVector = FALSE),
      error = function(e) list()
    )
  })

  out$spec_json <- NULL

  list(artifacts = purrr::transpose(as.list(out)))
}

# -- Rerender artifact --------------------------------------------------------

rerender_visual_artifact <- function(
  con,
  artifact_id,
  overrides = list(),
  add_data_view = FALSE
) {
  artifact_id <- normalize_viz_scalar(artifact_id)
  if (is.na(artifact_id)) {
    stop("'artifact_id' is required.", call. = FALSE)
  }

  current <- get_visual_artifact_metadata(
    con = con,
    artifact_ids = list(artifact_id),
    include_inactive = TRUE
  )

  if (length(current$artifacts) == 0) {
    stop(paste0("Unknown artifact_id: ", artifact_id, "."), call. = FALSE)
  }

  artifact <- current$artifacts[[1]]
  spec <- artifact$spec %||% list()
  merged <- modifyList(spec, overrides %||% list())

  # Code-based artifact
  if (
    !is.null(merged$plot_code) &&
      is.character(merged$plot_code) &&
      identical(merged$mode, "code")
  ) {
    return(run_create_plot_code(
      con = con,
      plot_code = merged$plot_code,
      table_names = merged$table_names %||% merged$table_name,
      description = merged$description %||% "Rerendered artifact",
      inspiration_schemas = merged$inspiration_schemas %||% c("basic"),
      artifact_name = artifact_id,
      artifact_label = merged$artifact_label %||% artifact$artifact_label,
      title = merged$title,
      subtitle = merged$subtitle,
      width = merged$width %||% 9,
      height = merged$height %||% 5,
      dpi = merged$dpi %||% 180,
      limit_rows = merged$limit_rows %||% 50000L,
      add_data_view = isTRUE(add_data_view)
    ))
  }

  # Schema-based artifact
  if (!is.null(merged$schema_name) && is.character(merged$schema_name)) {
    return(run_create_plot_from_schema(
      con = con,
      schema_name = merged$schema_name,
      table_name = merged$table_name,
      column_map = merged$column_map %||% list(),
      description = merged$description %||% "Rerendered artifact",
      title = merged$title,
      subtitle = merged$subtitle,
      artifact_name = artifact_id,
      artifact_label = merged$artifact_label %||% artifact$artifact_label,
      width = merged$width %||% 9,
      height = merged$height %||% 5,
      dpi = merged$dpi %||% 180,
      add_data_view = isTRUE(add_data_view)
    ))
  }

  stop(
    paste0("Cannot rerender artifact '", artifact_id, "': unrecognized mode."),
    call. = FALSE
  )
}
