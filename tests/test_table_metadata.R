# table_metadata helper tests
#
# Run from project root with:
# Rscript tests/test_table_metadata.R

library(testthat)
library(DBI)
library(duckdb)
library(tibble)

source(file.path("R", "table_metadata.R"))

describe("get_table_profile", {
  test_that("returns deep profile for one table", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

    DBI::dbWriteTable(
      con,
      "profile_tbl",
      tibble(
        id = 1:4,
        all_null = c(NA, NA, NA, NA),
        blank_text = c("", " ", NA, ""),
        category = c("A", "B", "A", NA),
        measure = c(1, 2, 2, NA)
      ),
      overwrite = TRUE
    )

    profile <- get_table_profile(con, "profile_tbl")

    expect_equal(profile$table_name, "profile_tbl")
    expect_equal(profile$row_count, 4)
    expect_equal(profile$column_count, 5)
    expect_true(all(
      c("all_null", "blank_text") %in% profile$fully_missing_columns
    ))
    expect_true("id" %in% profile$likely_identifier_columns)
    expect_true("category" %in% profile$likely_categorical_columns)
    expect_true(profile$summary$fully_missing_column_count >= 2)
    expect_true(profile$summary$likely_identifier_column_count >= 1)

    cols <- profile$columns
    names(cols) <- vapply(cols, `[[`, character(1), "column_name")

    sorted_names <- vapply(profile$columns, `[[`, character(1), "column_name")
    expect_true(sorted_names[[1]] %in% c("all_null", "blank_text"))

    expect_equal(cols$all_null$non_missing_count, 0)
    expect_equal(cols$blank_text$blank_count, 3)
    expect_equal(cols$category$distinct_non_missing_count, 2)
    expect_true(isTRUE(cols$id$likely_identifier))
    expect_true(isTRUE(cols$category$likely_categorical))
    expect_true(length(cols$category$sample_unique_values) <= 3)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("respects sample_values_n bounds", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

    DBI::dbWriteTable(
      con,
      "many_values",
      tibble(x = as.character(1:100)),
      overwrite = TRUE
    )

    profile <- get_table_profile(
      con,
      "many_values",
      sample_values_n = 999,
      max_sample_chars = 50
    )

    expect_equal(profile$sample_values_n, 20)
    expect_true(length(profile$columns[[1]]$sample_unique_values) <= 20)

    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  test_that("errors for unknown table", {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

    expect_error(
      get_table_profile(con, "missing_table"),
      "Table not found"
    )

    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})
