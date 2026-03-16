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
