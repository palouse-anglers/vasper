# WSU AgWeatherNet API Tests
#
# Run from project root with:
# Rscript tests/test_wsu_awn_api.R

library(testthat)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(withr)

source(file.path("R", "wsu_awn_api.R"))

make_record <- function(timestamp, at_f, rh = "54.5", precip = "0.00") {
  list(
    AT1_F = "NA",
    AT_F = at_f,
    DEWPT_F = "37.4",
    LW_UNITY = "NA",
    MSLP_HPA = "1016",
    P_INCHES = precip,
    RH_PCNT = rh,
    SM8_PCNT = "NA",
    SR_WM2 = "0",
    ST2_F = "61.0",
    ST8_F = "61.0",
    SWP8_KPA = "-589.9",
    TIMESTAMP_PST = timestamp,
    WD_DEGREE = "164",
    WS_MAX_MPH = "14.8",
    WS_MPH = "6.3"
  )
}

describe("flatten_wsu_weather_records()", {
  test_that("parses values, NA sentinels, timestamps, and frost flag", {
    response <- list(
      status = 1,
      message = list(
        list(
          STATION_ID = 100326,
          STATION_NAME = "Hogeye",
          DATA = list(
            make_record("2026-06-05 00:00:00", "53.4"),
            make_record("2026-06-04 23:45:00", "30.0")
          )
        )
      )
    )

    result <- flatten_wsu_weather_records(response)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)

    # Ascending order by timestamp (API returns descending)
    expect_true(result$timestamp[[1]] < result$timestamp[[2]])

    expect_equal(result$station_id[[1]], 100326L)
    expect_equal(result$station_name[[1]], "Hogeye")

    # Numeric coercion (rows are sorted ascending by timestamp, so the
    # 2026-06-04 23:45 record comes before the 2026-06-05 00:00 record)
    expect_type(result$at_f, "double")
    expect_equal(result$at_f, c(30.0, 53.4))

    # "NA" sentinel becomes real NA
    expect_true(all(is.na(result$at1_f)))
    expect_true(all(is.na(result$sm8_pcnt)))

    # Derived columns
    expect_true(all(
      c("timestamp", "date", "time_day_of_week", "frost") %in% names(result)
    ))
    expect_equal(result$frost, c(TRUE, FALSE))
    expect_s3_class(result$date, "Date")

    # Raw timestamp string preserved
    expect_true("timestamp_pst" %in% names(result))
  })

  test_that("returns a typed empty tibble when DATA is empty", {
    response <- list(
      status = 1,
      message = list(
        list(
          STATION_ID = 100329,
          STATION_NAME = "Alto",
          DATA = list()
        )
      )
    )

    result <- flatten_wsu_weather_records(response)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_true(all(WSU_AWN_MEASUREMENT_FIELDS %in% names(result)))
    expect_true(all(
      c("station_id", "station_name", "timestamp", "frost") %in% names(result)
    ))
  })
})

describe("wsu_chr_to_num()", {
  test_that("maps 'NA' strings to NA and parses numbers", {
    expect_equal(wsu_chr_to_num(c("53.4", "NA", "0.00")), c(53.4, NA, 0))
  })
})

describe("get_weather_historical_wsu() validation", {
  test_that("rejects date ranges longer than the per-call cap", {
    withr::local_envvar(c(
      WSU_AWN_USERNAME = "user",
      WSU_AWN_PASSWORD = "pass"
    ))

    expect_error(
      get_weather_historical_wsu(
        station_id = 100326,
        date_range = c("2026-01-01", "2026-06-01")
      ),
      "92 days"
    )
  })

  test_that("rejects invalid resolution values", {
    withr::local_envvar(c(
      WSU_AWN_USERNAME = "user",
      WSU_AWN_PASSWORD = "pass"
    ))

    expect_error(
      get_weather_historical_wsu(
        station_id = 100326,
        date_range = c("2026-01-01", "2026-01-02"),
        resolution = "YEARLY"
      ),
      "resolution"
    )
  })

  test_that("requires credentials before performing a request", {
    withr::local_envvar(c(
      WSU_AWN_USERNAME = "",
      WSU_AWN_PASSWORD = ""
    ))

    expect_error(
      get_weather_historical_wsu(
        station_id = 100326,
        date_range = c("2026-01-01", "2026-01-02")
      ),
      "Missing WSU AgWeatherNet credentials"
    )
  })
})

describe("get_weather_stations_wsu()", {
  test_that("requires credentials", {
    withr::local_envvar(c(
      WSU_AWN_USERNAME = "",
      WSU_AWN_PASSWORD = ""
    ))

    expect_error(
      get_weather_stations_wsu(),
      "Missing WSU AgWeatherNet credentials"
    )
  })
})

describe("WSU_AWN_TARGET_STATIONS", {
  test_that("contains the three granted Columbia County stations", {
    expect_equal(
      WSU_AWN_TARGET_STATIONS,
      c(Hogeye = 100326L, Jackson = 100328L, Alto = 100329L)
    )
  })
})
