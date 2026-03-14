# Weather API Tests
# Test suite for Open-Meteo API integration

library(testthat)
library(httr2)
library(tibble)

source("global.R")

# Test configuration
test_lat <- 46.73
test_lon <- -117.18
test_location_name <- "Pullman, WA"

describe("API Direct Access (Baseline)", {
  test_that("Forecast API is accessible and returns data", {
    req <- request("https://api.open-meteo.com/v1/forecast") |>
      req_url_query(
        latitude = test_lat,
        longitude = test_lon,
        daily = "temperature_2m_max",
        temperature_unit = "fahrenheit"
      )

    resp <- req_perform(req)
    expect_equal(resp$status_code, 200)

    data <- resp_body_json(resp, simplifyVector = TRUE)
    expect_true("daily" %in% names(data))
    expect_true("time" %in% names(data$daily))
  })

  test_that("Historical/Archive API is accessible with correct URL", {
    req <- request("https://archive-api.open-meteo.com/v1/archive") |>
      req_url_query(
        latitude = test_lat,
        longitude = test_lon,
        start_date = "2025-01-01",
        end_date = "2025-01-07",
        daily = "temperature_2m_max",
        temperature_unit = "fahrenheit"
      )

    resp <- req_perform(req)
    expect_equal(resp$status_code, 200)

    data <- resp_body_json(resp, simplifyVector = TRUE)
    expect_equal(length(data$daily$time), 7)
  })

  test_that("Soil variables are available with correct names", {
    req <- request("https://api.open-meteo.com/v1/forecast") |>
      req_url_query(
        latitude = test_lat,
        longitude = test_lon,
        daily = "soil_temperature_0_to_7cm_mean,soil_moisture_0_to_7cm_mean",
        temperature_unit = "fahrenheit"
      )

    resp <- req_perform(req)
    expect_equal(resp$status_code, 200)

    data <- resp_body_json(resp, simplifyVector = TRUE)
    expect_true("soil_temperature_0_to_7cm_mean" %in% names(data$daily))
    expect_true("soil_moisture_0_to_7cm_mean" %in% names(data$daily))
  })
})

describe("get_weather_data() - Core Function", {
  test_that("retrieves forecast data", {
    result <- get_weather_data(
      latitude = test_lat,
      longitude = test_lon,
      date_range = c(Sys.Date(), Sys.Date() + 6),
      type = "forecast"
    )

    expect_s3_class(result, "tbl_df")
    expect_true("time" %in% names(result))
    expect_true("temperature_2m_max" %in% names(result))
    expect_gte(nrow(result), 7)
  })

  test_that("retrieves historical data", {
    result <- get_weather_data(
      latitude = test_lat,
      longitude = test_lon,
      date_range = c("2025-01-01", "2025-01-31"),
      variables = c(
        "temperature_2m_max",
        "temperature_2m_min",
        "precipitation_sum"
      ),
      type = "historical"
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 31)
    expect_true("temperature_2m_max" %in% names(result))
    expect_true("precipitation_sum" %in% names(result))
  })

  test_that("validates latitude bounds", {
    expect_error(
      get_weather_data(91, test_lon, c("2025-01-01", "2025-01-07")),
      "latitude"
    )

    expect_error(
      get_weather_data(-91, test_lon, c("2025-01-01", "2025-01-07")),
      "latitude"
    )
  })

  test_that("validates longitude bounds", {
    expect_error(
      get_weather_data(test_lat, 181, c("2025-01-01", "2025-01-07")),
      "longitude"
    )

    expect_error(
      get_weather_data(test_lat, -181, c("2025-01-01", "2025-01-07")),
      "longitude"
    )
  })

  test_that("includes location in output", {
    result <- get_weather_data(
      latitude = test_lat,
      longitude = test_lon,
      date_range = c("2025-01-01", "2025-01-07"),
      type = "historical"
    )

    expect_true("latitude" %in% names(result))
    expect_true("longitude" %in% names(result))
    expect_equal(unique(result$latitude), test_lat)
    expect_equal(unique(result$longitude), test_lon)
  })
})

describe("get_n_day_forecast() - Simplified Forecast", {
  test_that("returns requested number of days", {
    result <- get_n_day_forecast(test_lat, test_lon, n = 7)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 7)
  })

  test_that("includes today in results", {
    result <- get_n_day_forecast(test_lat, test_lon, n = 7)

    expect_equal(min(result$time), Sys.Date())
  })

  test_that("respects maximum of 16 days", {
    expect_error(
      get_n_day_forecast(test_lat, test_lon, n = 17),
      "16"
    )
  })

  test_that("retrieves soil variables", {
    result <- get_n_day_forecast(
      test_lat,
      test_lon,
      n = 7,
      variables = c(
        "temperature_2m_max",
        "temperature_2m_min",
        "soil_temperature_0_to_7cm_mean",
        "soil_moisture_0_to_7cm_mean"
      )
    )

    expect_true("soil_temperature_0_to_7cm_mean" %in% names(result))
    expect_true("soil_moisture_0_to_7cm_mean" %in% names(result))
  })

  test_that("retrieves all requested variables", {
    result <- get_n_day_forecast(
      test_lat,
      test_lon,
      n = 7,
      variables = c(
        "temperature_2m_max",
        "temperature_2m_min",
        "precipitation_sum",
        "wind_speed_10m_max"
      )
    )

    expect_true(all(
      c("temperature_2m_max", "precipitation_sum", "wind_speed_10m_max") %in%
        names(result)
    ))
  })
})

describe("Database Integration", {
  test_that("write_weather_to_db() creates table", {
    # Setup test database
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    # Create sample data
    test_data <- tibble(
      time = seq(Sys.Date(), Sys.Date() + 6, by = "day"),
      temperature_2m_max = rnorm(7, mean = 50, sd = 10),
      latitude = test_lat,
      longitude = test_lon
    )

    # Write to database
    result <- write_weather_to_db(
      data = test_data,
      con = con,
      tool_name = "forecast",
      param_hash = "test1234",
      table_label = "Test Forecast Table"
    )

    expect_true("table_name" %in% names(result))
    expect_equal(result$row_count, 7)
    expect_true(DBI::dbExistsTable(con, result$table_name))

    md <- get_table_metadata(con, include_tables = c("table_metadata"))
    expect_true(result$table_name %in% md$table_name)
    expect_equal(
      md$table_label[match(result$table_name, md$table_name)],
      "Test Forecast Table"
    )

    # Cleanup
    DBI::dbDisconnect(con)
  })

  test_that("write_weather_to_db() caches data (doesn't overwrite)", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    test_data <- tibble(
      time = Sys.Date(),
      temperature_2m_max = 50,
      latitude = test_lat,
      longitude = test_lon
    )

    # Write once
    result1 <- write_weather_to_db(
      test_data,
      con,
      "forecast",
      "cachtest",
      "Cached Forecast"
    )

    # Try to write again - should use cached version
    result2 <- write_weather_to_db(
      test_data,
      con,
      "forecast",
      "cachtest",
      "Cached Forecast"
    )

    expect_equal(result1$table_name, result2$table_name)

    DBI::dbDisconnect(con)
  })
})

describe("Edge Cases and Error Handling", {
  test_that("get_n_day_forecast() rejects negative days", {
    expect_error(
      get_n_day_forecast(test_lat, test_lon, n = -1),
      "n"
    )
  })

  test_that("get_n_day_forecast() rejects zero days", {
    expect_error(
      get_n_day_forecast(test_lat, test_lon, n = 0),
      "n"
    )
  })

  test_that("Functions handle missing/NULL coordinates", {
    expect_error(
      get_n_day_forecast(NULL, test_lon, n = 7)
    )

    expect_error(
      get_n_day_forecast(test_lat, NULL, n = 7)
    )
  })
})
