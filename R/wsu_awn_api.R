# WSU AgWeatherNet (AWN) API integration
#
# Station Data API service docs (provided by WSU):
#   Base URL:  https://weather.wsu.edu
#   Endpoints: /webservice/stationdata, /webservice/metadata
#   Auth:      UNAME + PASS query parameters
#   Response:  JSON by default; { "status": 1, "message": [...] } on success,
#              { "status": -1, "message": "<error text>" } on failure.
#
# Scope notes (verified against the granted Palouse Anglers account):
# - Implemented: /metadata (station metadata) and /stationdata (interval data).
# - The three target stations are meter2 sensors in Columbia County, WA:
#     Hogeye (100326), Jackson (100328), Alto (100329).
# - DATA_RESOLUTION / BASIS is effectively ignored for these stations: 15MIN,
#   HOURLY, and DAILY all return native 15-minute interval records, and MONTHLY
#   returns no rows. We therefore land native 15-minute records in DuckDB and let
#   query_tables SQL roll them up (hourly/daily/monthly) as needed.

# Target WSU AgWeatherNet stations granted to this account (Columbia County, WA).
WSU_AWN_TARGET_STATIONS <- c(
  Hogeye = 100326L,
  Jackson = 100328L,
  Alto = 100329L
)

# Measurement fields returned in each stationdata DATA record.
# Used to build consistent (possibly empty) result frames.
WSU_AWN_MEASUREMENT_FIELDS <- c(
  "at_f", # Air temperature (degrees F)
  "at1_f", # Secondary air temperature sensor (degrees F)
  "dewpt_f", # Dew point (degrees F)
  "rh_pcnt", # Relative humidity (%)
  "p_inches", # Precipitation (inches)
  "ws_mph", # Wind speed (mph)
  "ws_max_mph", # Maximum wind gust (mph)
  "wd_degree", # Wind direction (degrees)
  "sr_wm2", # Solar radiation (W/m^2)
  "st2_f", # Soil temperature at 2 inches (degrees F)
  "st8_f", # Soil temperature at 8 inches (degrees F)
  "sm8_pcnt", # Soil moisture at 8 inches (%)
  "swp8_kpa", # Soil water potential at 8 inches (kPa)
  "mslp_hpa", # Mean sea-level pressure (hPa)
  "lw_unity" # Leaf wetness (unitless)
)

#' Get WSU AgWeatherNet credentials from environment
#'
#' @return Named list with username and password
#' @export
get_wsu_credentials <- function() {
  username <- Sys.getenv("WSU_AWN_USERNAME")
  password <- Sys.getenv("WSU_AWN_PASSWORD")

  if (!nzchar(username) || !nzchar(password)) {
    stop(
      paste(
        "Missing WSU AgWeatherNet credentials.",
        "Set WSU_AWN_USERNAME and WSU_AWN_PASSWORD in .Renviron."
      ),
      call. = FALSE
    )
  }

  list(username = username, password = password)
}

#' Build and execute a WSU AgWeatherNet API request
#'
#' Adds credentials and FORMAT=JSON, performs the request with retries, and
#' validates the API-level status field (-1 indicates an error with a string
#' message; 1 indicates success with a list message).
#'
#' @param path API path beginning with '/'
#' @param query Named list of additional query parameters
#'
#' @return Parsed JSON list
#' @export
request_wsu_api <- function(path, query = list()) {
  creds <- get_wsu_credentials()

  base_url <- "https://weather.wsu.edu/webservice"

  # WSU serves native 15-minute records and can be slow for wide date ranges
  # (a ~6-month pull was observed taking ~60s), so gateway timeouts (HTTP 504)
  # and other transient gateway errors are common. httr2's default retry only
  # treats 429/503 as transient, so we extend it to cover 408/500/502/503/504
  # and connection failures, with a bounded per-request timeout and backoff.
  is_transient_wsu <- function(resp) {
    httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
  }

  resp <- httr2::request(paste0(base_url, path)) |>
    httr2::req_url_query(
      UNAME = creds$username,
      PASS = creds$password,
      FORMAT = "JSON",
      !!!query
    ) |>
    httr2::req_timeout(seconds = 90) |>
    httr2::req_retry(
      max_tries = 4,
      max_seconds = 240,
      retry_on_failure = TRUE,
      is_transient = is_transient_wsu,
      backoff = function(attempt) min(2^attempt, 20)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  status <- resp$status
  if (is.null(status) || !isTRUE(as.integer(status) == 1L)) {
    detail <- resp$message
    if (is.character(detail) && length(detail) == 1) {
      stop(
        paste0("WSU AgWeatherNet API error: ", detail),
        call. = FALSE
      )
    }
    stop(
      "WSU AgWeatherNet API returned a non-success status.",
      call. = FALSE
    )
  }

  resp
}

#' Coerce a WSU API string value to numeric
#'
#' WSU returns measurements as strings and uses the literal "NA" for missing
#' values.
#'
#' @param x Character vector
#'
#' @return Numeric vector with "NA" mapped to NA
#' @export
wsu_chr_to_num <- function(x) {
  suppressWarnings(as.numeric(dplyr::na_if(as.character(x), "NA")))
}

#' Flatten a WSU stationdata response into a tibble
#'
#' @param response Parsed JSON response from /stationdata
#' @param station_id Optional station id fallback when the header omits it
#'
#' @return Tibble with one row per interval record (ascending by timestamp)
#' @export
flatten_wsu_weather_records <- function(response, station_id = NULL) {
  message_list <- response$message
  header <- if (length(message_list) >= 1) message_list[[1]] else list()

  station_id_val <- header$STATION_ID
  if (is.null(station_id_val)) {
    station_id_val <- station_id
  }
  station_id_val <- if (is.null(station_id_val)) {
    NA_integer_
  } else {
    as.integer(station_id_val)
  }

  station_name_val <- header$STATION_NAME
  if (is.null(station_name_val)) {
    station_name_val <- NA_character_
  }

  records <- header$DATA
  if (is.null(records)) {
    records <- list()
  }

  empty_result <- function() {
    measurements <- stats::setNames(
      lapply(WSU_AWN_MEASUREMENT_FIELDS, function(...) numeric(0)),
      WSU_AWN_MEASUREMENT_FIELDS
    )

    tibble::as_tibble(c(
      list(
        station_id = integer(0),
        station_name = character(0),
        timestamp = as.POSIXct(character(0), tz = "Etc/GMT+8"),
        date = as.Date(character(0)),
        time_day_of_week = character(0),
        timestamp_pst = character(0)
      ),
      measurements,
      list(frost = logical(0))
    ))
  }

  if (length(records) == 0) {
    return(empty_result())
  }

  raw <- purrr::map_dfr(records, function(record) {
    record[vapply(record, is.null, logical(1))] <- NA_character_
    tibble::as_tibble(lapply(record, as.character))
  })

  raw <- dplyr::rename_with(raw, tolower)

  numeric_cols <- setdiff(names(raw), "timestamp_pst")
  raw <- raw |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(numeric_cols), wsu_chr_to_num)
    )

  # Ensure all expected measurement columns exist even if the API omits one.
  for (field in WSU_AWN_MEASUREMENT_FIELDS) {
    if (!field %in% names(raw)) {
      raw[[field]] <- NA_real_
    }
  }

  raw |>
    dplyr::mutate(
      station_id = station_id_val,
      station_name = station_name_val,
      # WSU reports the TIMESTAMP_PST field in fixed Pacific Standard Time
      # (UTC-8) year-round with no daylight saving, matching the field name and
      # avoiding twice-yearly time-series discontinuities. Etc/GMT+8 encodes
      # fixed UTC-8 (note: the sign is inverted in the Etc/ zone naming).
      timestamp = lubridate::ymd_hms(
        timestamp_pst,
        tz = "Etc/GMT+8",
        quiet = TRUE
      ),
      date = as.Date(timestamp),
      time_day_of_week = weekdays(date, abbreviate = FALSE),
      frost = at_f <= 32
    ) |>
    dplyr::arrange(timestamp) |>
    dplyr::relocate(
      station_id,
      station_name,
      timestamp,
      date,
      time_day_of_week,
      timestamp_pst,
      dplyr::all_of(WSU_AWN_MEASUREMENT_FIELDS),
      frost
    )
}

#' Get WSU AgWeatherNet station metadata
#'
#' Fetches the full public metadata catalog and filters to the granted target
#' stations by default.
#'
#' @param station_ids Integer vector of station ids to keep (default: the three
#'   granted Columbia County stations). Use NULL to return all stations.
#'
#' @return Tibble with station metadata
#' @export
get_weather_stations_wsu <- function(station_ids = WSU_AWN_TARGET_STATIONS) {
  response <- request_wsu_api("/metadata")

  stations <- response$message
  if (is.null(stations)) {
    stations <- list()
  }

  keep_ids <- if (is.null(station_ids)) NULL else as.integer(station_ids)

  rows <- purrr::map_dfr(stations, function(station) {
    sid <- station$STATION_ID
    if (is.null(sid)) {
      return(tibble::tibble())
    }
    sid <- as.integer(sid)

    if (!is.null(keep_ids) && !(sid %in% keep_ids)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      station_id = sid,
      station_name = station$STATION_NAME %||% NA_character_,
      county = station$COUNTY %||% NA_character_,
      city = station$CITY %||% NA_character_,
      state = station$STATE %||% NA_character_,
      latitude = wsu_chr_to_num(station$LATITUDE_DEGREE %||% NA),
      longitude = wsu_chr_to_num(station$LONGITUDE_DEGREE %||% NA),
      elevation_ft = wsu_chr_to_num(station$ELEVATION_FEET %||% NA),
      station_type = station$STATION_TYPE %||% NA_character_,
      tier = station$TIER %||% NA_character_,
      active = station$ACTIVE_STATION %||% NA_character_,
      visibility = station$STATION_VISIBILITY %||% NA_character_,
      installation_date = station$INSTALLATION_DATE %||% NA_character_
    )
  })

  if (nrow(rows) > 0) {
    rows <- dplyr::arrange(rows, station_id)
  }

  rows
}

#' Get historical WSU AgWeatherNet interval data for one station
#'
#' Returns native 15-minute interval records. For hourly/daily/monthly summaries,
#' aggregate the resulting table with query_tables SQL.
#'
#' @param station_id Single WSU station id (for example 100326)
#' @param date_range Character vector of length 2: c(start, end). Dates may be
#'   "YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS".
#' @param resolution Requested API resolution passed through as DATA_RESOLUTION.
#'   Defaults to "15MIN"; note granted meter2 stations serve native 15-minute
#'   data regardless of this value.
#'
#' @return Tibble with interval weather records
#' @export
get_weather_historical_wsu <- function(
  station_id,
  date_range,
  resolution = "15MIN"
) {
  stopifnot(length(station_id) == 1)
  station_id <- as.integer(station_id)
  if (is.na(station_id)) {
    stop("'station_id' must be a valid integer station id.", call. = FALSE)
  }

  stopifnot(length(date_range) == 2)
  start <- trimws(as.character(date_range[[1]]))
  end <- trimws(as.character(date_range[[2]]))

  resolution <- toupper(trimws(as.character(resolution[[1]])))
  valid_resolutions <- c("5MIN", "15MIN", "HOURLY", "DAILY", "MONTHLY")
  if (!resolution %in% valid_resolutions) {
    stop(
      paste0(
        "'resolution' must be one of: ",
        paste(valid_resolutions, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  start_date <- as.Date(substr(start, 1, 10))
  end_date <- as.Date(substr(end, 1, 10))
  if (is.na(start_date) || is.na(end_date)) {
    stop(
      "'date_range' values must parse as dates (YYYY-MM-DD).",
      call. = FALSE
    )
  }

  span_days <- as.numeric(difftime(end_date, start_date, units = "days"))
  if (span_days < 0) {
    stop("'date_range' must be c(start, end) with start <= end.", call. = FALSE)
  }
  # WSU returns native 15-minute records (~96 rows/day). Wide ranges are slow on
  # the server and risk HTTP 504 gateway timeouts, so cap each call to roughly a
  # quarter and direct callers to make repeated calls for longer history. For
  # multi-year climate analysis, prefer get_weather_historical_open_meteo.
  max_span_days <- 92
  if (span_days > max_span_days) {
    stop(
      paste0(
        "'date_range' is limited to ",
        max_span_days,
        " days per call (15-minute data is large and the WSU endpoint is slow ",
        "for wide ranges); make repeated calls for longer history, or use ",
        "get_weather_historical_open_meteo for multi-year climate ranges."
      ),
      call. = FALSE
    )
  }

  # Sub-daily resolutions (5MIN/15MIN/HOURLY) require full
  # "YYYY-MM-DD HH:MM:SS" bounds; date-only inputs are otherwise ignored and the
  # API returns only the latest record. Expand date-only inputs to a full-day
  # window so callers can pass simple dates.
  expand_bound <- function(value, end_of_day) {
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
      suffix <- if (end_of_day) " 23:59:59" else " 00:00:00"
      paste0(value, suffix)
    } else {
      value
    }
  }

  if (resolution %in% c("5MIN", "15MIN", "HOURLY")) {
    start <- expand_bound(start, end_of_day = FALSE)
    end <- expand_bound(end, end_of_day = TRUE)
  }

  response <- request_wsu_api(
    path = "/stationdata",
    query = list(
      STATION_ID = station_id,
      DATA_RESOLUTION = resolution,
      START = start,
      END = end
    )
  )

  flatten_wsu_weather_records(response, station_id = station_id)
}
