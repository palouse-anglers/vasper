# Seed Variety Trial Helpers ----
#
# Wraps the pre-loaded `seed_variety_trials` DuckDB table (populated at startup
# from data/seed_variety_trials.csv) with a tool the AI assistant can call
# to look up 2023 Dayton and Walla Walla WSU/NWGG winter wheat variety
# performance data.
#
# Trial data source: Northwest Grain Growers / WSU, via
#   https://www.nwgrgr.com/fccp-seed-21683
#
# Parse script: data-raw/parse_seed_trial_data.R
# Data file:    data/seed_variety_trials.csv

SEED_TRIAL_CSV_PATH <- file.path("data", "seed_variety_trials.csv")

# Valid columns for ORDER BY (allowlist prevents SQL injection)
SEED_TRIAL_ORDER_COLS <- c(
  "yield_bu_ac",
  "protein_pct",
  "test_weight_lb_bu",
  "height_in",
  "maturity_days",
  "variety",
  "trial_name",
  "location"
)

#' Load seed variety trial data into DuckDB at startup
#'
#' Creates the `seed_variety_trials` table in the provided connection from the
#' committed CSV.  Registers metadata and returns invisibly.
#'
#' @param con DBI connection (in-memory DuckDB)
#' @param table_name Table name to create (default: TABLE_NAMES$seed_variety_trials)
#' @param csv_path Path to the seed trial CSV (default: SEED_TRIAL_CSV_PATH)
#'
#' @return Invisibly TRUE on success, invisibly FALSE when CSV is not found.
#' @export
load_seed_variety_trials <- function(
  con,
  table_name = TABLE_NAMES$seed_variety_trials,
  csv_path = SEED_TRIAL_CSV_PATH
) {
  if (!file.exists(csv_path)) {
    warning(
      sprintf(
        paste0(
          "Seed trial data not found at '%s'. ",
          "Run data-raw/parse_seed_trial_data.R to generate it."
        ),
        csv_path
      ),
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE TABLE {table_name} AS ",
      "SELECT * FROM read_csv_auto('{csv_path}')"
    )
  )

  upsert_table_metadata(
    con = con,
    table_name = table_name,
    table_label = "Seed Variety Trials (2023 WSU/NWGG)",
    source = "startup",
    source_detail = paste0(
      "Loaded from ",
      csv_path,
      "; source: Northwest Grain Growers / WSU 2023 winter wheat variety trials"
    ),
    row_count = DBI::dbGetQuery(
      con,
      glue::glue("SELECT COUNT(*) AS n FROM {table_name}")
    )$n[[1]],
    column_count = length(DBI::dbListFields(con, table_name)),
    is_active = TRUE
  )

  invisible(TRUE)
}

#' Query seed variety trial data with optional filters
#'
#' Core implementation backing the `get_seed_variety_trials` tool. Builds a
#' parameterised DuckDB SQL query from the provided filter arguments, executes
#' it, and optionally persists the result as a named table with metadata.
#'
#' @param con DBI connection
#' @param table_name Source table name (default: "seed_variety_trials")
#' @param wheat_class Character: "HRW" or "SWW", or NULL for all
#' @param location Character partial-match filter on location, or NULL
#' @param variety_pattern Character partial-match filter on variety, or NULL
#' @param min_yield Numeric minimum yield (bu/ac), or NULL
#' @param max_yield Numeric maximum yield (bu/ac), or NULL
#' @param min_protein Numeric minimum protein (%), or NULL
#' @param max_protein Numeric maximum protein (%), or NULL
#' @param order_by Column to sort by (must be in SEED_TRIAL_ORDER_COLS)
#' @param descending Logical sort direction (default TRUE)
#' @param limit Integer maximum rows (default 30, capped at 200)
#' @param table_label User-facing label for persisted output table
#' @param add_data_view Logical whether to persist result as a data view
#'
#' @return Named list with `rows` (integer), `data` (data.frame), and
#'   optionally `table_name` and `table_label` when `add_data_view` is TRUE
#'   and results are non-empty.
#' @export
run_seed_variety_trial_query <- function(
  con,
  table_name = "seed_variety_trials",
  wheat_class = NULL,
  location = NULL,
  variety_pattern = NULL,
  min_yield = NULL,
  max_yield = NULL,
  min_protein = NULL,
  max_protein = NULL,
  order_by = "yield_bu_ac",
  descending = TRUE,
  limit = 30L,
  table_label = NULL,
  add_data_view = TRUE
) {
  # Build WHERE clause from filters
  conditions <- character()

  if (!is.null(wheat_class) && nzchar(trimws(as.character(wheat_class)))) {
    wc <- trimws(toupper(as.character(wheat_class)))
    conditions <- c(conditions, sprintf("UPPER(wheat_class) = '%s'", wc))
  }

  if (!is.null(location) && nzchar(trimws(as.character(location)))) {
    loc <- gsub("'", "''", trimws(as.character(location)))
    conditions <- c(conditions, sprintf("location ILIKE '%%%s%%'", loc))
  }

  if (
    !is.null(variety_pattern) && nzchar(trimws(as.character(variety_pattern)))
  ) {
    pat <- gsub("'", "''", trimws(as.character(variety_pattern)))
    conditions <- c(conditions, sprintf("variety ILIKE '%%%s%%'", pat))
  }

  if (!is.null(min_yield)) {
    conditions <- c(
      conditions,
      sprintf("yield_bu_ac >= %s", as.numeric(min_yield))
    )
  }

  if (!is.null(max_yield)) {
    conditions <- c(
      conditions,
      sprintf("yield_bu_ac <= %s", as.numeric(max_yield))
    )
  }

  if (!is.null(min_protein)) {
    conditions <- c(
      conditions,
      sprintf("protein_pct >= %s", as.numeric(min_protein))
    )
  }

  if (!is.null(max_protein)) {
    conditions <- c(
      conditions,
      sprintf("protein_pct <= %s", as.numeric(max_protein))
    )
  }

  where_clause <- if (length(conditions) > 0) {
    paste("WHERE", paste(conditions, collapse = " AND "))
  } else {
    ""
  }

  # Sanitize ORDER BY against allowlist to prevent injection
  order_col <- trimws(as.character(order_by %||% "yield_bu_ac"))
  if (!order_col %in% SEED_TRIAL_ORDER_COLS) {
    order_col <- "yield_bu_ac"
  }
  order_dir <- if (isTRUE(descending)) "DESC" else "ASC"

  limit_n <- as.integer(limit %||% 30L)
  if (is.na(limit_n) || limit_n < 1L) {
    limit_n <- 30L
  }
  limit_n <- min(limit_n, 200L)

  sql <- sprintf(
    "SELECT variety, wheat_class, location, trial_name, trial_year,
            yield_bu_ac, test_weight_lb_bu, protein_pct, height_in,
            maturity_days, planted_date, harvested_date, source_url
     FROM %s
     %s
     ORDER BY %s %s
     LIMIT %d",
    table_name,
    where_clause,
    order_col,
    order_dir,
    limit_n
  )

  result <- tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(e) {
      stop(
        paste("Failed to query seed variety trials:", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  out <- list(
    rows = nrow(result),
    data = result
  )

  if (isTRUE(add_data_view) && nrow(result) > 0) {
    label <- trimws(as.character(table_label %||% "Seed Variety Trials"))
    if (!nzchar(label)) {
      label <- "Seed Variety Trials"
    }

    scope_hash <- digest::digest(
      list(
        wheat_class,
        location,
        variety_pattern,
        min_yield,
        max_yield,
        min_protein,
        max_protein,
        order_col,
        order_dir,
        limit_n
      ),
      algo = "crc32"
    )
    out_table <- paste0("seed_trial_query__", scope_hash)

    tryCatch(
      {
        if (!DBI::dbExistsTable(con, out_table)) {
          DBI::dbWriteTable(con, out_table, result, overwrite = FALSE)
          upsert_table_metadata(
            con = con,
            table_name = out_table,
            table_label = label,
            source = "seed_trial_tool",
            source_detail = paste0(
              "tool=get_seed_variety_trials; filters: ",
              if (length(conditions) > 0) {
                paste(conditions, collapse = ", ")
              } else {
                "none"
              }
            ),
            row_count = nrow(result),
            column_count = ncol(result),
            is_active = TRUE
          )
          out$table_name <- out_table
          out$table_label <- label
          out$add_data_view <- TRUE
        }
      },
      error = function(e) NULL
    )
  }

  out
}
