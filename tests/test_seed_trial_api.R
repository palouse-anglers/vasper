# Seed variety trial helper tests
#
# Run from project root with:
# Rscript tests/test_seed_trial_api.R

library(testthat)
library(DBI)
library(duckdb)
library(tibble)

source(file.path("R", "table_metadata.R"))
source(file.path("R", "seed_trial_api.R"))

# ---- Fixture helpers ----

# Minimal seed trial fixture that mirrors the CSV schema.
make_seed_fixture <- function() {
  tibble(
    variety = c("LCS Missile", "Keldin", "LCS Kamiak", "Scorpio", "Canvas"),
    wheat_class = c("HRW", "HRW", "SWW", "HRW", "HRW"),
    location = c(
      "Dayton, WA",
      "Dayton, WA",
      "Dayton, WA",
      "Walla Walla, WA",
      "Dayton, WA"
    ),
    trial_name = c(
      "2023 Dayton Winter Wheat Trial - HRW",
      "2023 Dayton Winter Wheat Trial - HRW",
      "2023 Dayton Winter Wheat Trial",
      "2023 Walla Walla Winter Wheat Trial - HRW",
      "2023 Dayton Winter Wheat Trial - HRW"
    ),
    trial_year = c(2023L, 2023L, 2023L, 2023L, 2023L),
    yield_bu_ac = c(86, 84, 87, 84, 75),
    test_weight_lb_bu = c(60.6, 61.8, 60.4, 61.6, 62.5),
    protein_pct = c(12.3, 12.2, 11.7, 12.3, 11.6),
    height_in = c(31L, 31L, 29L, 30L, 30L),
    maturity_days = c(143L, 143L, 143L, 143L, 143L),
    planted_date = rep("10/6/2022", 5),
    harvested_date = c(rep("8/3/2023", 4), "8/3/2023"),
    source = rep("test", 5),
    source_url = rep("https://inetsgi.com/customer/780/fa395e67.pdf", 5)
  )
}

make_test_con <- function(fixture = make_seed_fixture()) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  ensure_table_metadata(con)
  DBI::dbWriteTable(con, "seed_variety_trials", fixture, overwrite = TRUE)
  con
}

# ---- load_seed_variety_trials ----

describe("load_seed_variety_trials", {
  test_that("warns and returns FALSE when CSV is missing", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    expect_warning(
      result <- load_seed_variety_trials(
        con,
        table_name = "seed_variety_trials",
        csv_path = tempfile(fileext = ".csv") # guaranteed non-existent
      ),
      "not found"
    )
    expect_false(isTRUE(result))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("loads CSV and registers metadata when file exists", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    load_seed_variety_trials(
      con,
      table_name = "seed_variety_trials",
      csv_path = SEED_TRIAL_CSV_PATH
    )

    expect_true(DBI::dbExistsTable(con, "seed_variety_trials"))

    n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM seed_variety_trials")$n
    expect_true(n > 0L)

    md <- DBI::dbGetQuery(
      con,
      "SELECT table_label, source FROM table_metadata WHERE table_name = 'seed_variety_trials'"
    )
    expect_equal(nrow(md), 1L)
    expect_match(md$table_label, "Seed Variety Trials", fixed = TRUE)
    expect_equal(md$source, "startup")

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("loaded table has all expected columns", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    load_seed_variety_trials(
      con,
      table_name = "seed_variety_trials",
      csv_path = SEED_TRIAL_CSV_PATH
    )

    cols <- DBI::dbListFields(con, "seed_variety_trials")
    expected <- c(
      "variety",
      "yield_bu_ac",
      "test_weight_lb_bu",
      "protein_pct",
      "height_in",
      "maturity_days",
      "trial_name",
      "trial_year",
      "location",
      "wheat_class",
      "planted_date",
      "harvested_date",
      "source",
      "source_url"
    )
    expect_true(all(expected %in% cols))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- run_seed_variety_trial_query: unfiltered ----

describe("run_seed_variety_trial_query unfiltered", {
  test_that("returns all rows up to limit when no filters are applied", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(con, add_data_view = FALSE)

    expect_equal(result$rows, 5L)
    expect_s3_class(result$data, "data.frame")
    expect_true("variety" %in% names(result$data))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("default sort is yield_bu_ac descending", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(con, add_data_view = FALSE)

    yields <- result$data$yield_bu_ac
    expect_equal(yields, sort(yields, decreasing = TRUE))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("result contains expected output columns", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(con, add_data_view = FALSE)

    expected_cols <- c(
      "variety",
      "wheat_class",
      "location",
      "trial_name",
      "trial_year",
      "yield_bu_ac",
      "test_weight_lb_bu",
      "protein_pct",
      "height_in",
      "maturity_days",
      "planted_date",
      "harvested_date",
      "source_url"
    )
    expect_true(all(expected_cols %in% names(result$data)))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- run_seed_variety_trial_query: filters ----

describe("run_seed_variety_trial_query filters", {
  test_that("wheat_class filter is case-insensitive and exact", {
    con <- make_test_con()

    hrw <- run_seed_variety_trial_query(
      con,
      wheat_class = "hrw",
      add_data_view = FALSE
    )
    expect_true(all(hrw$data$wheat_class == "HRW"))
    expect_equal(hrw$rows, 4L)

    sww <- run_seed_variety_trial_query(
      con,
      wheat_class = "SWW",
      add_data_view = FALSE
    )
    expect_equal(sww$rows, 1L)
    expect_equal(sww$data$variety, "LCS Kamiak")

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("location filter is case-insensitive partial match", {
    con <- make_test_con()

    ww <- run_seed_variety_trial_query(
      con,
      location = "walla",
      add_data_view = FALSE
    )
    expect_equal(ww$rows, 1L)
    expect_equal(ww$data$variety, "Scorpio")

    dayton <- run_seed_variety_trial_query(
      con,
      location = "Dayton",
      add_data_view = FALSE
    )
    expect_equal(dayton$rows, 4L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("variety_pattern filter is case-insensitive partial match", {
    con <- make_test_con()

    lcs <- run_seed_variety_trial_query(
      con,
      variety_pattern = "LCS",
      add_data_view = FALSE
    )
    expect_equal(lcs$rows, 2L)
    expect_true(all(grepl("LCS", lcs$data$variety, ignore.case = TRUE)))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("min_yield filter excludes lower-yielding varieties", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      min_yield = 85,
      add_data_view = FALSE
    )
    expect_true(all(result$data$yield_bu_ac >= 85))
    expect_equal(result$rows, 2L) # 86 (LCS Missile) and 87 (LCS Kamiak)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("max_yield filter excludes higher-yielding varieties", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      max_yield = 80,
      add_data_view = FALSE
    )
    expect_true(all(result$data$yield_bu_ac <= 80))
    expect_equal(result$rows, 1L) # only Canvas at 75

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("min_protein and max_protein filters combine correctly", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      min_protein = 12.0,
      max_protein = 12.3,
      add_data_view = FALSE
    )
    expect_true(all(result$data$protein_pct >= 12.0))
    expect_true(all(result$data$protein_pct <= 12.3))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("combined filters narrow results correctly", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      location = "Dayton",
      min_yield = 84,
      add_data_view = FALSE
    )
    expect_true(all(result$data$wheat_class == "HRW"))
    expect_true(all(grepl("Dayton", result$data$location, ignore.case = TRUE)))
    expect_true(all(result$data$yield_bu_ac >= 84))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("returns zero rows (not an error) when no varieties match", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      min_yield = 999,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 0L)
    expect_equal(nrow(result$data), 0L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- run_seed_variety_trial_query: ordering ----

describe("run_seed_variety_trial_query ordering", {
  test_that("ascending sort returns lowest-yield first", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      order_by = "yield_bu_ac",
      descending = FALSE,
      add_data_view = FALSE
    )
    yields <- result$data$yield_bu_ac
    expect_equal(yields, sort(yields, decreasing = FALSE))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("order_by protein_pct returns correct order", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      order_by = "protein_pct",
      descending = TRUE,
      add_data_view = FALSE
    )
    proteins <- result$data$protein_pct
    expect_equal(proteins, sort(proteins, decreasing = TRUE))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("unrecognised order_by silently falls back to yield_bu_ac", {
    con <- make_test_con()

    # Should not error; ORDER BY falls back to yield_bu_ac DESC
    result <- run_seed_variety_trial_query(
      con,
      order_by = "DROP TABLE seed_variety_trials; --",
      add_data_view = FALSE
    )
    expect_equal(result$rows, 5L)
    yields <- result$data$yield_bu_ac
    expect_equal(yields, sort(yields, decreasing = TRUE))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- run_seed_variety_trial_query: limit ----

describe("run_seed_variety_trial_query limit", {
  test_that("limit parameter restricts rows returned", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      limit = 2L,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 2L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("limit is capped at 200", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      limit = 999L,
      add_data_view = FALSE
    )
    # Fixture only has 5 rows; capped limit returns all of them
    expect_equal(result$rows, 5L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("invalid limit defaults to 30", {
    con <- make_test_con()

    # NA limit — should default gracefully
    result <- run_seed_variety_trial_query(
      con,
      limit = NA_integer_,
      add_data_view = FALSE
    )
    expect_true(result$rows <= 30L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- run_seed_variety_trial_query: data view persistence ----

describe("run_seed_variety_trial_query data view persistence", {
  test_that("add_data_view=TRUE writes result table and metadata", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      table_label = "HRW Trial Results",
      add_data_view = TRUE
    )

    expect_true(!is.null(result$table_name))
    expect_true(DBI::dbExistsTable(con, result$table_name))
    expect_equal(result$table_label, "HRW Trial Results")

    md <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT table_label, source FROM table_metadata WHERE table_name = '%s'",
        result$table_name
      )
    )
    expect_equal(nrow(md), 1L)
    expect_equal(md$source, "seed_trial_tool")

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("add_data_view=FALSE does not write any extra table", {
    con <- make_test_con()

    tables_before <- DBI::dbListTables(con)
    run_seed_variety_trial_query(con, add_data_view = FALSE)
    tables_after <- DBI::dbListTables(con)

    new_tables <- setdiff(tables_after, tables_before)
    expect_equal(
      length(new_tables[startsWith(new_tables, "seed_trial_query__")]),
      0L
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("add_data_view=TRUE with no results does not write a table", {
    con <- make_test_con()

    tables_before <- DBI::dbListTables(con)
    run_seed_variety_trial_query(
      con,
      min_yield = 999,
      table_label = "Empty Result",
      add_data_view = TRUE
    )
    tables_after <- DBI::dbListTables(con)

    new_query_tables <- setdiff(tables_after, tables_before)
    expect_equal(
      length(new_query_tables[startsWith(
        new_query_tables,
        "seed_trial_query__"
      )]),
      0L
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("repeated identical queries do not duplicate the output table", {
    con <- make_test_con()

    r1 <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      table_label = "HRW Trials",
      add_data_view = TRUE
    )
    r2 <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      table_label = "HRW Trials",
      add_data_view = TRUE
    )

    # First call creates the table; second call detects it already exists and skips.
    expect_true(!is.null(r1$table_name))
    expect_null(r2$table_name) # second call skips the write

    # Exactly one metadata row exists for the output table
    table_count <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT COUNT(*) AS n FROM table_metadata WHERE table_name = '%s'",
        r1$table_name
      )
    )$n
    expect_equal(table_count, 1L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("default label is used when table_label is NULL", {
    con <- make_test_con()

    result <- run_seed_variety_trial_query(
      con,
      table_label = NULL,
      add_data_view = TRUE
    )
    expect_equal(result$table_label, "Seed Variety Trials")

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- SQL injection safety ----

describe("run_seed_variety_trial_query SQL safety", {
  test_that("single-quote in location is escaped and does not error", {
    con <- make_test_con()

    expect_no_error(
      result <- run_seed_variety_trial_query(
        con,
        location = "O'Brien County",
        add_data_view = FALSE
      )
    )
    expect_equal(result$rows, 0L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("single-quote in variety_pattern is escaped and does not error", {
    con <- make_test_con()

    expect_no_error(
      result <- run_seed_variety_trial_query(
        con,
        variety_pattern = "'; DROP TABLE seed_variety_trials; --",
        add_data_view = FALSE
      )
    )
    # Table should still exist and be queryable
    expect_true(DBI::dbExistsTable(con, "seed_variety_trials"))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

# ---- Integration: actual CSV data ----

describe("run_seed_variety_trial_query against real CSV", {
  test_that("returns 119 total varieties when loaded from the committed CSV", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)
    load_seed_variety_trials(con, table_name = "seed_variety_trials")

    result <- run_seed_variety_trial_query(
      con,
      limit = 200L,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 119L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("HRW class contains 54 varieties (24 Dayton + 30 Walla Walla)", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)
    load_seed_variety_trials(con, table_name = "seed_variety_trials")

    result <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      limit = 200L,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 54L)
    expect_true(all(result$data$wheat_class == "HRW"))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("SWW class contains 65 varieties from Dayton trial", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)
    load_seed_variety_trials(con, table_name = "seed_variety_trials")

    result <- run_seed_variety_trial_query(
      con,
      wheat_class = "SWW",
      limit = 200L,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 65L)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("no rows have NA in key numeric columns", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)
    load_seed_variety_trials(con, table_name = "seed_variety_trials")

    result <- run_seed_variety_trial_query(
      con,
      limit = 200L,
      add_data_view = FALSE
    )
    df <- result$data

    expect_false(anyNA(df$yield_bu_ac))
    expect_false(anyNA(df$protein_pct))
    expect_false(anyNA(df$test_weight_lb_bu))

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("top HRW variety by yield at Dayton is LCS Missile at 86 bu/ac", {
    skip_if_not(file.exists(SEED_TRIAL_CSV_PATH))

    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)
    load_seed_variety_trials(con, table_name = "seed_variety_trials")

    result <- run_seed_variety_trial_query(
      con,
      wheat_class = "HRW",
      location = "Dayton",
      limit = 1L,
      add_data_view = FALSE
    )
    expect_equal(result$rows, 1L)
    expect_equal(result$data$variety, "LCS Missile")
    expect_equal(result$data$yield_bu_ac, 86)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})
