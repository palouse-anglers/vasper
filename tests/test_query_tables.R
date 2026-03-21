# query_tables tool helper tests
#
# Run from project root with:
# Rscript tests/test_query_tables.R

library(testthat)
library(DBI)
library(duckdb)
library(tibble)

source(file.path("R", "table_metadata.R"))
source(file.path("R", "chat_helpers.R"))
source(file.path("R", "query_tables.R"))

describe("run_query_tables mode validation", {
  test_that("sql-only input defaults to free mode", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "a", tibble(x = 1:3), overwrite = TRUE)

    result <- run_query_tables(
      con = con,
      sql = "SELECT x FROM a ORDER BY x",
      persist = FALSE
    )

    expect_equal(result$mode, "free")
    expect_equal(result$dimensions$nrow, 3)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("free mode rejects input_tables", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "a", tibble(x = 1:3), overwrite = TRUE)

    expect_error(
      run_query_tables(
        con = con,
        mode = "free",
        sql = "SELECT * FROM a",
        input_tables = c("a")
      ),
      "does not allow 'input_tables'"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("free mode validates referenced FROM tables", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "existing_table", tibble(x = 1:3), overwrite = TRUE)
    upsert_table_metadata(
      con = con,
      table_name = "existing_table",
      table_label = "Existing",
      source = "test",
      row_count = 3L,
      column_count = 1L,
      is_active = TRUE
    )

    expect_error(
      run_query_tables(
        con = con,
        mode = "free",
        sql = "SELECT * FROM missing_table"
      ),
      "Referenced table\\(s\\) not found"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("free mode ignores CTE names in table validation", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(
      con,
      "usda_yields_trend_c126a819",
      tibble(
        year = c(2020L, 2021L),
        commodity_desc = c("WHEAT", "BARLEY"),
        statisticcat_desc = c("YIELD", "YIELD"),
        unit_desc = c("BU / ACRE", "BU / ACRE"),
        value_mean = c(60, 55),
        rows_numeric = c(1L, 1L)
      ),
      overwrite = TRUE
    )

    result <- run_query_tables(
      con = con,
      mode = "free",
      sql = "
        WITH wheat_totals AS (
          SELECT year, 'WHEAT' AS commodity_desc, statisticcat_desc, unit_desc,
                 SUM(value_mean * rows_numeric) / SUM(rows_numeric) AS value_mean
          FROM usda_yields_trend_c126a819
          WHERE commodity_desc = 'WHEAT'
          GROUP BY year, statisticcat_desc, unit_desc
        ),
        barley_data AS (
          SELECT year, commodity_desc, statisticcat_desc, unit_desc, value_mean
          FROM usda_yields_trend_c126a819
          WHERE commodity_desc = 'BARLEY'
        )
        SELECT * FROM wheat_totals
        UNION ALL
        SELECT * FROM barley_data
      "
    )

    expect_equal(result$mode, "free")
    expect_true(result$dimensions$nrow >= 1)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("vectorized mode rejects FROM in sql", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "a", tibble(x = 1:3), overwrite = TRUE)

    expect_error(
      run_query_tables(
        con = con,
        mode = "vectorized",
        sql = "SELECT * FROM a",
        input_tables = c("a"),
        output_table_names = c("out_a"),
        output_table_labels = c("Out A")
      ),
      "cannot include FROM"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("vectorized mode enforces matching lengths", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "a", tibble(x = 1:3), overwrite = TRUE)
    DBI::dbWriteTable(con, "b", tibble(x = 4:6), overwrite = TRUE)

    expect_error(
      run_query_tables(
        con = con,
        mode = "vectorized",
        sql = "WHERE x > 2",
        input_tables = c("a", "b"),
        output_table_names = c("out_a"),
        output_table_labels = c("Out A", "Out B")
      ),
      "Input lengths must match"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("destructive SQL is blocked in free mode", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    expect_error(
      run_query_tables(
        con = con,
        mode = "free",
        sql = "DROP TABLE some_table"
      ),
      "Destructive SQL statements are not allowed"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

describe("run_query_tables execution", {
  test_that("free mode persist writes table and metadata", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(
      con,
      "soil_data",
      tibble(year = c(2020L, 2021L), value = c(10, 20)),
      overwrite = TRUE
    )

    result <- run_query_tables(
      con = con,
      mode = "free",
      sql = "SELECT year, value FROM soil_data",
      output_table_names = c("soil_subset"),
      output_table_labels = c("Soil Subset"),
      persist = TRUE,
      add_data_view = TRUE
    )

    expect_equal(result$table_name, "soil_subset")
    expect_equal(result$table_label, "Soil Subset")
    expect_true(DBI::dbExistsTable(con, "soil_subset"))

    md <- get_table_metadata(con, include_tables = c("table_metadata"))
    expect_true("soil_subset" %in% md$table_name)
    expect_equal(
      md$source[match("soil_subset", md$table_name)],
      "query_tables"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("vectorized mode persist writes one output per input", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(con, "tbl_a", tibble(x = c(1, 2, 3)), overwrite = TRUE)
    DBI::dbWriteTable(con, "tbl_b", tibble(x = c(3, 4, 5)), overwrite = TRUE)

    result <- run_query_tables(
      con = con,
      mode = "vectorized",
      sql = "WHERE x >= 3",
      input_tables = c("tbl_a", "tbl_b"),
      output_table_names = c("out_a", "out_b"),
      output_table_labels = c("Out A", "Out B"),
      persist = TRUE,
      add_data_view = TRUE
    )

    expect_equal(result$table_name, c("out_a", "out_b"))
    expect_true(DBI::dbExistsTable(con, "out_a"))
    expect_true(DBI::dbExistsTable(con, "out_b"))
    expect_equal(length(result$variable_names), 2)
    expect_equal(length(result$dimensions), 2)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("free mode non-persist returns in-memory rows and dimensions", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(
      con,
      "weather_test",
      tibble(day = 1:3, rain = c(0, 0.2, 0.1)),
      overwrite = TRUE
    )

    result <- run_query_tables(
      con = con,
      mode = "free",
      sql = "SELECT day, rain FROM weather_test ORDER BY day",
      persist = FALSE,
      max_rows = 10
    )

    expect_equal(result$dimensions$nrow, 3)
    expect_equal(result$dimensions$ncol, 2)
    expect_equal(result$rows_returned, 3)
    expect_false(result$result_truncated)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("free mode surfaces UNION ORDER BY guidance", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    ensure_table_metadata(con)

    DBI::dbWriteTable(
      con,
      "union_test",
      tibble(crop = c("A", "B"), v = c(1, 2)),
      overwrite = TRUE
    )

    expect_error(
      run_query_tables(
        con = con,
        mode = "free",
        sql = "
          SELECT crop, v FROM union_test
          UNION ALL
          SELECT crop, v FROM union_test
          ORDER BY CASE WHEN crop = 'A' THEN 1 ELSE 2 END
        "
      ),
      "Tip: For UNION queries"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})
