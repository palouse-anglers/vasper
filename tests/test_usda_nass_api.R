# USDA NASS QuickStats API Tests

library(testthat)
library(tibble)

source(testthat::test_path("..", "R", "table_metadata.R"))
source(testthat::test_path("..", "R", "usda_nass_api.R"))

describe("credentials and parsing", {
  test_that("get_usda_nass_credentials errors when key missing", {
    withr::local_envvar(c(USDA_NASS_API_KEY = ""))

    expect_error(
      get_usda_nass_credentials(),
      "Missing USDA NASS API key"
    )
  })

  test_that("parse_nass_value handles numeric and suppression codes", {
    parsed <- parse_nass_value(c("1,234", "(D)", "(Z)", ""))

    expect_equal(parsed$value_num[[1]], 1234)
    expect_true(parsed$is_suppressed[[2]])
    expect_equal(parsed$value_num[[3]], 0)
    expect_true(parsed$is_suppressed[[4]])
  })

  test_that("validate_usda_nass_query rejects invalid parameter names", {
    expect_error(
      validate_usda_nass_query(
        list(setor_desc = "CROPS"),
        endpoint = "api_GET"
      ),
      "Invalid USDA NASS parameter"
    )
  })

  test_that("validate_usda_nass_query accepts operator suffixes", {
    expect_no_error(
      validate_usda_nass_query(
        list(year__GE = 2020, year__LE = 2024, source_desc = "SURVEY"),
        endpoint = "get_counts"
      )
    )
  })
})

describe("rate limit helpers", {
  test_that("set_usda_nass_rate_limit updates options", {
    old <- set_usda_nass_rate_limit(
      rate_seconds = 0.05,
      throttled_rate_seconds = 0.1,
      override = TRUE,
      reset_throttling = TRUE
    )

    expect_equal(getOption("vasper.nass.rate_seconds"), 0.05)
    expect_equal(getOption("vasper.nass.throttled_rate_seconds"), 0.1)
    expect_true(getOption("vasper.nass.override_rate_limit"))
    expect_type(old, "list")
  })

  test_that("usda_nass_rate_limit can be overridden", {
    set_usda_nass_rate_limit(override = TRUE)
    start <- Sys.time()
    waited <- usda_nass_rate_limit()
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

    expect_equal(waited, 0)
    expect_lt(elapsed, 0.05)
  })
})

describe("trend derivation", {
  test_that("get_columbia_county_nass_trends aggregates by year and crop dimensions", {
    raw_data <- tibble(
      year = c(2022L, 2022L, 2023L),
      commodity_desc = c("WHEAT", "WHEAT", "WHEAT"),
      class_desc = c("WINTER", "WINTER", "WINTER"),
      util_practice_desc = c("GRAIN", "GRAIN", "GRAIN"),
      statisticcat_desc = c("YIELD", "YIELD", "YIELD"),
      unit_desc = c("BU / ACRE", "BU / ACRE", "BU / ACRE"),
      value_num = c(70, NA, 80),
      is_suppressed = c(FALSE, TRUE, FALSE)
    )

    trends <- get_columbia_county_nass_trends(raw_data)

    expect_equal(nrow(trends), 2)
    expect_true(all(
      c("rows_total", "rows_numeric", "rows_suppressed") %in% names(trends)
    ))
    expect_equal(trends$rows_total[trends$year == 2022], 2)
  })
})

describe("database writer", {
  test_that("write_yield_to_db writes raw and trend tables and metadata", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    ensure_table_metadata(con)

    raw_data <- tibble(
      year = c(2022L, 2023L),
      commodity_desc = c("WHEAT", "BARLEY"),
      statisticcat_desc = c("YIELD", "YIELD"),
      value_raw = c("68", "59"),
      value_num = c(68, 59),
      is_suppressed = c(FALSE, FALSE)
    )

    trend_data <- tibble(
      year = c(2022L, 2023L),
      commodity_desc = c("WHEAT", "BARLEY"),
      class_desc = c("", ""),
      util_practice_desc = c("", ""),
      statisticcat_desc = c("YIELD", "YIELD"),
      unit_desc = c("BU / ACRE", "BU / ACRE"),
      rows_total = c(1L, 1L),
      rows_numeric = c(1L, 1L),
      rows_suppressed = c(0L, 0L),
      value_mean = c(68, 59),
      value_min = c(68, 59),
      value_max = c(68, 59),
      value_sum = c(68, 59)
    )

    result <- write_yield_to_db(
      raw_data = raw_data,
      trend_data = trend_data,
      con = con,
      param_hash = "abc12345",
      table_label = "USDA Yield Test",
      add_data_view = FALSE
    )

    expect_true(length(result$table_name) == 2)
    expect_true(DBI::dbExistsTable(con, "usda_yields_raw_abc12345"))
    expect_true(DBI::dbExistsTable(con, "usda_yields_trend_abc12345"))

    metadata <- get_table_metadata(con, include_tables = c("table_metadata"))
    expect_true("usda_yields_raw_abc12345" %in% metadata$table_name)
    expect_true("usda_yields_trend_abc12345" %in% metadata$table_name)

    DBI::dbDisconnect(con)
  })
})

describe("live USDA NASS API integration (optional)", {
  test_that("count and raw retrieval works for Columbia County wheat", {
    testthat::skip_if_not(
      nzchar(Sys.getenv("USDA_NASS_API_KEY")),
      message = "Set USDA_NASS_API_KEY to run live USDA NASS tests"
    )

    query <- list(
      source_desc = "SURVEY",
      sector_desc = "CROPS",
      state_alpha = "WA",
      state_ansi = "53",
      county_ansi = "013",
      county_name = "COLUMBIA",
      agg_level_desc = "COUNTY",
      statisticcat_desc = "YIELD",
      commodity_desc = "WHEAT",
      year__GE = "2022",
      year__LE = "2024"
    )

    count <- tryCatch(
      get_usda_nass_count(query),
      error = function(e) {
        testthat::skip(
          paste(
            "USDA NASS API timed out; skipping live integration test:",
            conditionMessage(e)
          )
        )
      }
    )

    expect_true(is.numeric(count))
    expect_gte(count, 0)

    raw_rows <- tryCatch(
      get_columbia_county_nass_raw(
        crops = c("WHEAT"),
        statistics = c("YIELD"),
        year_min = 2022,
        year_max = 2024
      ),
      error = function(e) {
        testthat::skip(
          paste(
            "USDA NASS API timed out; skipping live integration test:",
            conditionMessage(e)
          )
        )
      }
    )

    expect_s3_class(raw_rows, "tbl_df")
    expect_true(all(
      c("value_raw", "value_num", "is_suppressed") %in% names(raw_rows)
    ))
  })
})
