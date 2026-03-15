# Davis WeatherLink API Tests

library(testthat)
library(dplyr)
library(purrr)
library(tibble)

source(testthat::test_path("..", "R", "davis_api.R"))

describe("flatten_davis_weather_records()", {
  test_that("flattens sensor rows and derives realts", {
    response <- list(
      station_id = 12345,
      generated_at = 1710000000,
      sensors = list(
        list(
          lsid = 10,
          sensor_type = 31,
          data_structure_type = 6,
          data = list(
            list(
              ts = 1710000060,
              tz_offset = -28800,
              temp_out = 50.5,
              hum_out = 60
            )
          )
        )
      )
    )

    result <- flatten_davis_weather_records(
      response = response,
      station_uuid = "station-uuid-1",
      sensor_types = c(31)
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
    expect_true("realts" %in% names(result))
    expect_true("temp_out" %in% names(result))
    expect_equal(result$station_uuid[[1]], "station-uuid-1")
    expect_equal(result$sensor_type[[1]], 31)
  })

  test_that("handles missing data rows", {
    response <- list(
      station_id = 12345,
      generated_at = 1710000000,
      sensors = list(
        list(
          lsid = 10,
          sensor_type = 31,
          data_structure_type = 6,
          data = list()
        )
      )
    )

    result <- flatten_davis_weather_records(
      response = response,
      station_uuid = "station-uuid-1"
    )

    expect_equal(nrow(result), 1)
    expect_true(all(c("ts", "tz_offset", "realts") %in% names(result)))
  })
})

describe("get_weather_historical_davis() validation", {
  test_that("enforces max 24 hour window", {
    expect_error(
      get_weather_historical_davis(
        station_uuid = "station-uuid-1",
        start_timestamp = 1710000000,
        end_timestamp = 1710000000 + (24 * 60 * 60) + 1
      )
    )
  })
})

describe("station identifier coercion", {
  test_that("get_weather_current_davis coerces numeric station id to character", {
    withr::local_envvar(
      c(
        DAVIS_API_KEY = "",
        DAVIS_API_SECRET = ""
      )
    )

    expect_error(
      get_weather_current_davis(
        station_uuid = 12345,
        sensor_types = c(31)
      ),
      "Missing Davis credentials"
    )
  })

  test_that("get_weather_historical_davis coerces numeric station id to character", {
    withr::local_envvar(
      c(
        DAVIS_API_KEY = "",
        DAVIS_API_SECRET = ""
      )
    )

    expect_error(
      get_weather_historical_davis(
        station_uuid = 12345,
        start_timestamp = 1710000000,
        end_timestamp = 1710003600,
        sensor_types = c(31)
      ),
      "Missing Davis credentials"
    )
  })
})

describe("Live Davis API integration (optional)", {
  test_that("stations endpoint returns station metadata", {
    testthat::skip_if_not(
      nzchar(Sys.getenv("DAVIS_API_KEY")) &&
        nzchar(Sys.getenv("DAVIS_API_SECRET")),
      message = "Set DAVIS_API_KEY and DAVIS_API_SECRET to run live Davis tests"
    )

    stations <- get_weather_stations_davis()

    expect_s3_class(stations, "tbl_df")
    expect_true("station_uuid" %in% names(stations))
  })

  test_that("current endpoint works for an explicit station_uuid", {
    testthat::skip_if_not(
      nzchar(Sys.getenv("DAVIS_API_KEY")) &&
        nzchar(Sys.getenv("DAVIS_API_SECRET")),
      message = "Set DAVIS_API_KEY and DAVIS_API_SECRET to run live Davis tests"
    )

    stations <- get_weather_stations_davis()
    testthat::skip_if(
      nrow(stations) == 0,
      "No stations available to this API key"
    )

    station_uuid <- stations$station_uuid[[1]]

    current <- get_weather_current_davis(
      station_uuid = station_uuid,
      sensor_types = c(31)
    )

    expect_s3_class(current, "tbl_df")
    expect_true("station_uuid" %in% names(current))
  })

  test_that("historic endpoint works for explicit station_uuid and time window", {
    testthat::skip_if_not(
      nzchar(Sys.getenv("DAVIS_API_KEY")) &&
        nzchar(Sys.getenv("DAVIS_API_SECRET")),
      message = "Set DAVIS_API_KEY and DAVIS_API_SECRET to run live Davis tests"
    )

    stations <- get_weather_stations_davis()
    testthat::skip_if(
      nrow(stations) == 0,
      "No stations available to this API key"
    )

    station_uuid <- stations$station_uuid[[1]]
    end_ts <- as.integer(Sys.time()) - (60 * 60)
    start_ts <- end_ts - (60 * 60)

    historic <- get_weather_historical_davis(
      station_uuid = station_uuid,
      start_timestamp = start_ts,
      end_timestamp = end_ts,
      sensor_types = c(31)
    )

    expect_s3_class(historic, "tbl_df")
    expect_true("station_uuid" %in% names(historic))
  })
})
