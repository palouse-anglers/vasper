# Table Metadata Helpers ----

#' Ensure the table metadata registry exists
#'
#' @param con DBI connection
#'
#' @return Invisibly TRUE
#' @export
ensure_table_metadata <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS table_metadata (
      table_name VARCHAR PRIMARY KEY,
      table_label VARCHAR NOT NULL,
      source VARCHAR,
      source_detail VARCHAR,
      row_count BIGINT,
      column_count INTEGER,
      is_active INTEGER DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
    "
  )

  cols <- tryCatch(
    DBI::dbGetQuery(con, "PRAGMA table_info('table_metadata')")$name,
    error = function(e) character()
  )

  if (!"source_detail" %in% cols) {
    DBI::dbExecute(
      con,
      "ALTER TABLE table_metadata ADD COLUMN source_detail VARCHAR"
    )
  }

  invisible(TRUE)
}

#' Upsert a table label/metadata row
#'
#' @param con DBI connection
#' @param table_name Database table name
#' @param table_label User-facing label
#' @param source Source tag for provenance
#' @param row_count Optional row count
#' @param column_count Optional column count
#' @param is_active Whether table is active/visible
#'
#' @return Invisibly TRUE
#' @export
upsert_table_metadata <- function(
  con,
  table_name,
  table_label,
  source = "system",
  source_detail = NA_character_,
  row_count = NA_integer_,
  column_count = NA_integer_,
  is_active = TRUE
) {
  ensure_table_metadata(con)

  DBI::dbExecute(
    con,
    "DELETE FROM table_metadata WHERE table_name = ?",
    params = list(table_name)
  )

  DBI::dbExecute(
    con,
    "
    INSERT INTO table_metadata (
      table_name,
      table_label,
      source,
      source_detail,
      row_count,
      column_count,
      is_active,
      created_at,
      updated_at
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
    ",
    params = list(
      table_name,
      table_label,
      source,
      source_detail,
      row_count,
      column_count,
      as.integer(is_active)
    )
  )

  invisible(TRUE)
}

#' Synchronize table_metadata with currently available tables
#'
#' @param con DBI connection
#' @param include_tables Table names to force include
#' @param ignore_tables Table names to exclude
#'
#' @return Invisibly TRUE
#' @export
sync_table_metadata_with_db <- function(
  con,
  include_tables = c("table_metadata"),
  ignore_tables = character()
) {
  ensure_table_metadata(con)

  all_tables <- DBI::dbListTables(con)

  visible_tables <- union(setdiff(all_tables, ignore_tables), include_tables)

  # Mark rows as inactive when table no longer exists.
  DBI::dbExecute(
    con,
    "UPDATE table_metadata SET is_active = 0, updated_at = CURRENT_TIMESTAMP"
  )

  purrr::walk(visible_tables, function(tbl) {
    if (!DBI::dbExistsTable(con, tbl)) {
      return(invisible(NULL))
    }

    escaped_tbl <- as.character(DBI::dbQuoteIdentifier(con, tbl))

    row_count <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS n FROM ", escaped_tbl)
    )$n[[1]]

    column_count <- length(DBI::dbListFields(con, tbl))

    existing <- DBI::dbGetQuery(
      con,
      "SELECT table_label, source, source_detail FROM table_metadata WHERE table_name = ?",
      params = list(tbl)
    )

    table_label <- if (nrow(existing) > 0) {
      existing$table_label[[1]]
    } else {
      stringr::str_to_title(gsub("_", " ", tbl))
    }

    source_value <- if (
      nrow(existing) > 0 &&
        !is.na(existing$source[[1]]) &&
        nzchar(trimws(existing$source[[1]]))
    ) {
      existing$source[[1]]
    } else if (tbl %in% include_tables) {
      "system"
    } else {
      "sync"
    }

    source_detail_value <- if (
      nrow(existing) > 0 &&
        !is.na(existing$source_detail[[1]]) &&
        nzchar(trimws(existing$source_detail[[1]]))
    ) {
      existing$source_detail[[1]]
    } else {
      NA_character_
    }

    upsert_table_metadata(
      con = con,
      table_name = tbl,
      table_label = table_label,
      source = source_value,
      source_detail = source_detail_value,
      row_count = row_count,
      column_count = column_count,
      is_active = TRUE
    )
  })

  invisible(TRUE)
}

#' Read active table metadata
#'
#' @param con DBI connection
#' @param include_tables Table names to force include
#' @param ignore_tables Table names to exclude
#'
#' @return Tibble of table metadata
#' @export
get_table_metadata <- function(
  con,
  include_tables = c("table_metadata"),
  ignore_tables = character()
) {
  sync_table_metadata_with_db(
    con = con,
    include_tables = include_tables,
    ignore_tables = ignore_tables
  )

  DBI::dbGetQuery(
    con,
    "
    SELECT
      table_name,
      table_label,
      source,
      source_detail,
      row_count,
      column_count,
      is_active,
      updated_at
    FROM table_metadata
    WHERE is_active = 1
    ORDER BY table_label, table_name
    "
  ) |>
    tibble::as_tibble()
}

#' List visible data tables for selectors
#'
#' @param con DBI connection
#' @param include_tables Table names to force include
#' @param ignore_tables Table names to exclude
#'
#' @return Character vector of table names
#' @export
list_data_tables <- function(
  con,
  include_tables = c("table_metadata"),
  ignore_tables = character()
) {
  md <- get_table_metadata(
    con = con,
    include_tables = include_tables,
    ignore_tables = ignore_tables
  )

  md$table_name
}

#' Profile one table for data understanding
#'
#' @param con DBI connection
#' @param table_name Table name to profile
#' @param sample_values_n Number of distinct sample values per column
#' @param max_sample_chars Maximum characters per sampled value
#'
#' @return Named list with table-level and column-level profiling details
#' @export
get_table_profile <- function(
  con,
  table_name,
  sample_values_n = 3L,
  max_sample_chars = 120L
) {
  if (is.null(table_name) || length(table_name) == 0) {
    table_name <- ""
  }
  table_name <- as.character(table_name[[1]])
  table_name <- trimws(table_name)

  if (!nzchar(table_name)) {
    stop("'table_name' is required.", call. = FALSE)
  }

  if (!DBI::dbExistsTable(con, table_name)) {
    stop(
      paste0("Table not found: '", table_name, "'."),
      call. = FALSE
    )
  }

  sample_values_n <- suppressWarnings(as.integer(sample_values_n)[[1]])
  if (is.na(sample_values_n)) {
    sample_values_n <- 3L
  }
  sample_values_n <- max(0L, min(sample_values_n, 20L))

  max_sample_chars <- suppressWarnings(as.integer(max_sample_chars)[[1]])
  if (is.na(max_sample_chars) || max_sample_chars < 16L) {
    max_sample_chars <- 120L
  }
  max_sample_chars <- min(max_sample_chars, 500L)

  escaped_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))

  row_count <- DBI::dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n FROM ", escaped_tbl)
  )$n[[1]]
  row_count <- as.integer(row_count)

  fields <- DBI::dbListFields(con, table_name)

  if (length(fields) == 0) {
    return(list(
      table_name = table_name,
      row_count = row_count,
      column_count = 0L,
      sample_values_n = sample_values_n,
      summary = list(
        profile_generated_at = format(
          Sys.time(),
          "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        fully_missing_column_count = 0L,
        high_missing_column_count = 0L,
        likely_identifier_column_count = 0L,
        likely_categorical_column_count = 0L,
        likely_constant_column_count = 0L
      ),
      fully_missing_columns = character(),
      high_missing_columns = character(),
      likely_identifier_columns = character(),
      likely_categorical_columns = character(),
      likely_constant_columns = character(),
      columns = list()
    ))
  }

  column_info <- tryCatch(
    {
      rs <- DBI::dbSendQuery(
        con,
        paste0("SELECT * FROM ", escaped_tbl, " LIMIT 0")
      )
      on.exit(DBI::dbClearResult(rs), add = TRUE)
      DBI::dbColumnInfo(rs)
    },
    error = function(e) {
      tibble::tibble(name = fields, type = NA_character_)
    }
  )

  if (!"name" %in% names(column_info)) {
    column_info$name <- fields
  }
  if (!"type" %in% names(column_info)) {
    column_info$type <- NA_character_
  }

  type_lookup <- setNames(
    as.character(column_info$type),
    as.character(column_info$name)
  )

  column_profiles <- lapply(fields, function(col) {
    escaped_col <- as.character(DBI::dbQuoteIdentifier(con, col))
    col_type <- type_lookup[[col]]
    if (is.null(col_type) || length(col_type) == 0) {
      col_type <- NA_character_
    }
    type_upper <- toupper(as.character(col_type))
    is_text <- grepl("CHAR|TEXT|STRING|VARCHAR", type_upper)
    is_numeric <- grepl(
      "INT|DOUBLE|FLOAT|DECIMAL|NUMERIC|REAL|HUGEINT|BIGINT|SMALLINT|TINYINT",
      type_upper
    )

    missing_predicate <- if (isTRUE(is_text)) {
      paste0(
        escaped_col,
        " IS NULL OR TRIM(CAST(",
        escaped_col,
        " AS VARCHAR)) = ''"
      )
    } else {
      paste0(escaped_col, " IS NULL")
    }

    stats_row <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT ",
        "SUM(CASE WHEN ",
        escaped_col,
        " IS NULL THEN 1 ELSE 0 END) AS null_count, ",
        if (isTRUE(is_text)) {
          paste0(
            "SUM(CASE WHEN ",
            escaped_col,
            " IS NOT NULL AND TRIM(CAST(",
            escaped_col,
            " AS VARCHAR)) = '' THEN 1 ELSE 0 END)"
          )
        } else {
          "0"
        },
        " AS blank_count, ",
        "SUM(CASE WHEN NOT(",
        missing_predicate,
        ") THEN 1 ELSE 0 END) AS non_missing_count, ",
        "COUNT(DISTINCT CASE WHEN NOT(",
        missing_predicate,
        ") THEN ",
        escaped_col,
        " END) AS distinct_non_missing_count ",
        "FROM ",
        escaped_tbl
      )
    )

    value_or_zero <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x)) {
        return(0L)
      }
      as.integer(x)
    }

    null_count <- value_or_zero(stats_row$null_count[[1]])
    blank_count <- value_or_zero(stats_row$blank_count[[1]])
    non_missing_count <- value_or_zero(stats_row$non_missing_count[[1]])
    distinct_non_missing_count <- value_or_zero(
      stats_row$distinct_non_missing_count[[1]]
    )

    sample_values <- character()
    if (sample_values_n > 0L && non_missing_count > 0L) {
      sampled <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT SUBSTR(CAST(",
          escaped_col,
          " AS VARCHAR), 1, ",
          max_sample_chars,
          ") AS value FROM ",
          escaped_tbl,
          " WHERE NOT(",
          missing_predicate,
          ") ",
          "ORDER BY value LIMIT ",
          sample_values_n
        )
      )

      if (nrow(sampled) > 0 && "value" %in% names(sampled)) {
        sample_values <- as.character(sampled$value)
      }
    }

    missing_count <- row_count - non_missing_count
    missing_pct <- if (row_count > 0L) {
      round(100 * missing_count / row_count, 2)
    } else {
      NA_real_
    }

    likely_identifier <-
      row_count > 1L &&
      non_missing_count == row_count &&
      distinct_non_missing_count == non_missing_count

    likely_constant <-
      non_missing_count > 0L &&
      distinct_non_missing_count <= 1L

    categorical_threshold <- max(
      2L,
      min(50L, as.integer(round(non_missing_count * 0.2)))
    )
    likely_categorical <-
      non_missing_count > 0L &&
      distinct_non_missing_count >= 2L &&
      distinct_non_missing_count <= categorical_threshold

    likely_numeric_measure <-
      isTRUE(is_numeric) &&
      !likely_identifier &&
      !likely_constant

    list(
      column_name = col,
      data_type = as.character(col_type),
      missing_count = missing_count,
      missing_pct = missing_pct,
      null_count = null_count,
      blank_count = blank_count,
      non_missing_count = non_missing_count,
      distinct_non_missing_count = distinct_non_missing_count,
      sample_unique_values = sample_values,
      all_values_missing = non_missing_count == 0L,
      high_missing = !is.na(missing_pct) && missing_pct >= 95,
      likely_identifier = likely_identifier,
      likely_constant = likely_constant,
      likely_categorical = likely_categorical,
      likely_numeric_measure = likely_numeric_measure
    )
  })

  if (length(column_profiles) > 1L) {
    sort_key <- vapply(column_profiles, function(x) x$missing_pct, numeric(1))
    sort_key[is.na(sort_key)] <- -Inf
    sort_names <- vapply(
      column_profiles,
      function(x) x$column_name,
      character(1)
    )
    order_idx <- order(-sort_key, sort_names)
    column_profiles <- column_profiles[order_idx]
  }

  pick_columns <- function(predicate) {
    out <- vapply(
      column_profiles,
      function(x) {
        if (isTRUE(predicate(x))) x$column_name else NA_character_
      },
      character(1)
    )
    out <- stats::na.omit(out)
    unname(as.character(out))
  }

  fully_missing_columns <- pick_columns(function(x) x$all_values_missing)
  high_missing_columns <- pick_columns(function(x) x$high_missing)
  likely_identifier_columns <- pick_columns(function(x) x$likely_identifier)
  likely_categorical_columns <- pick_columns(function(x) x$likely_categorical)
  likely_constant_columns <- pick_columns(function(x) x$likely_constant)

  list(
    table_name = table_name,
    row_count = row_count,
    column_count = length(fields),
    sample_values_n = sample_values_n,
    summary = list(
      profile_generated_at = format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      fully_missing_column_count = length(fully_missing_columns),
      high_missing_column_count = length(high_missing_columns),
      likely_identifier_column_count = length(likely_identifier_columns),
      likely_categorical_column_count = length(likely_categorical_columns),
      likely_constant_column_count = length(likely_constant_columns)
    ),
    fully_missing_columns = fully_missing_columns,
    high_missing_columns = high_missing_columns,
    likely_identifier_columns = likely_identifier_columns,
    likely_categorical_columns = likely_categorical_columns,
    likely_constant_columns = likely_constant_columns,
    columns = column_profiles
  )
}
