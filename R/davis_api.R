# Davis WeatherLink v2 API integration

# Davis WeatherLink endpoint scope:
# User and implemented: /stations, /current/{station-id}, /historic/{station-id}
# Highest user value for station selection, current conditions, and historical context.
# Not implemented:
# Diagnostics/admin candidates only: /sensors, /sensor-activity
# Keep diagnostics endpoints out of normal chat flows unless explicitly requested.
# Deferred for now: /stations/{station-ids} (low incremental value for chat workflows)
# Deferred for now: /sensor-catalog (large payload >2MB)
# Deferred for now: /report/et/{station-id} (failed)

#' Get Davis API credentials from environment
#'
#' @return Named list with api_key and api_secret
#' @export
get_davis_credentials <- function() {
  api_key <- Sys.getenv("DAVIS_API_KEY")
  api_secret <- Sys.getenv("DAVIS_API_SECRET")

  if (!nzchar(api_key) || !nzchar(api_secret)) {
    stop(
      "Missing Davis credentials. Set DAVIS_API_KEY and DAVIS_API_SECRET in .Renviron.",
      call. = FALSE
    )
  }

  list(api_key = api_key, api_secret = api_secret)
}

#' Build and execute a Davis WeatherLink API request
#'
#' @param path API path beginning with '/'
#' @param query Named list of query parameters
#'
#' @return Parsed JSON list
#' @export
request_davis_api <- function(path, query = list()) {
  creds <- get_davis_credentials()

  base_url <- "https://api.weatherlink.com/v2"
  timestamp_now <- as.character(as.integer(Sys.time()))

  req <- httr2::request(paste0(base_url, path)) |>
    httr2::req_url_query(
      `api-key` = creds$api_key,
      t = timestamp_now,
      !!!query
    ) |>
    httr2::req_headers(
      `X-Api-Secret` = creds$api_secret
    )

  req |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

#' Return fallback when value is NULL
#'
#' @param x Value to check
#' @param default Default value when x is NULL
#'
#' @return x when not NULL, otherwise default
#' @export
value_or <- function(x, default) {
  if (is.null(x)) default else x
}

#' Convert WeatherLink NULL fields to NA recursively
#'
#' @param x List-like object
#'
#' @return Object with NULL values replaced by NA
#' @export
replace_null_with_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  }

  if (!is.list(x)) {
    return(x)
  }

  purrr::imap(x, function(value, nm) {
    if (is.null(value)) {
      NA
    } else if (is.list(value)) {
      replace_null_with_na(value)
    } else {
      value
    }
  })
}

#' Flatten Davis WeatherLink sensor payload into a tibble
#'
#' @param response Parsed JSON response from /current or /historic
#' @param station_uuid Station UUID used for the query
#' @param sensor_types Optional integer vector to filter sensor types
#'
#' @return Tibble with one row per sensor data record
#' @export
flatten_davis_weather_records <- function(
  response,
  station_uuid,
  sensor_types = NULL
) {
  sensors <- value_or(response$sensors, list())

  rows <- purrr::map_dfr(sensors, function(sensor) {
    if (!is.null(sensor_types) && !(sensor$sensor_type %in% sensor_types)) {
      return(tibble::tibble())
    }

    data_rows <- value_or(sensor$data, list())

    if (length(data_rows) == 0) {
      return(
        tibble::tibble(
          station_uuid = station_uuid,
          station_id = value_or(response$station_id, NA),
          generated_at = value_or(response$generated_at, NA),
          lsid = value_or(sensor$lsid, NA),
          sensor_type = value_or(sensor$sensor_type, NA),
          data_structure_type = value_or(sensor$data_structure_type, NA),
          ts = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"),
          tz_offset = NA_real_,
          realts = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
        )
      )
    }

    purrr::imap_dfr(data_rows, function(record, idx) {
      record <- replace_null_with_na(record)

      base <- tibble::tibble(
        station_uuid = station_uuid,
        station_id = value_or(response$station_id, NA),
        generated_at = value_or(response$generated_at, NA),
        lsid = value_or(sensor$lsid, NA),
        sensor_type = value_or(sensor$sensor_type, NA),
        data_structure_type = value_or(sensor$data_structure_type, NA),
        record_index = as.integer(idx)
      )

      if (length(record) == 0) {
        details <- tibble::tibble(
          ts = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"),
          tz_offset = NA_real_
        )
      } else {
        details <- tibble::as_tibble(record)
      }

      if ("ts" %in% names(details)) {
        details <- details |>
          dplyr::mutate(
            ts = as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
          )
      } else {
        details <- details |>
          dplyr::mutate(
            ts = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
          )
      }

      if (!"tz_offset" %in% names(details)) {
        details <- details |>
          dplyr::mutate(tz_offset = NA_real_)
      }

      dplyr::bind_cols(base, details) |>
        dplyr::mutate(
          generated_at = as.POSIXct(
            generated_at,
            origin = "1970-01-01",
            tz = "UTC"
          ),
          realts = as.POSIXct(
            as.numeric(ts) + as.numeric(tz_offset),
            origin = "1970-01-01",
            tz = "UTC"
          )
        ) |>
        dplyr::relocate(realts, .before = ts)
    })
  })

  rows
}

#' Get stations from Davis WeatherLink API
#'
#' @return Tibble with station metadata
#' @export
get_weather_stations_davis <- function() {
  response <- request_davis_api("/stations")

  stations <- value_or(response$stations, list())

  purrr::map_dfr(stations, function(station) {
    tz <- value_or(station$time_zone, "UTC")

    tibble::tibble(
      station_id = value_or(station$station_id, NA),
      station_uuid = value_or(station$station_id_uuid, NA),
      station_name = value_or(station$station_name, NA),
      gateway_id = value_or(station$gateway_id, NA),
      gateway_id_hex = value_or(station$gateway_id_hex, NA),
      product_number = value_or(station$product_number, NA),
      username = value_or(station$username, NA),
      user_email = value_or(station$user_email, NA),
      company_name = value_or(station$company_name, NA),
      active = value_or(station$active, NA),
      private = value_or(station$private, NA),
      recording_interval = value_or(station$recording_interval, NA),
      firmware_version = value_or(station$firmware_version, NA),
      imei = value_or(station$imei, NA),
      registered_date = as.POSIXct(
        value_or(station$registered_date, NA),
        origin = "1970-01-01",
        tz = tz
      ),
      subscription_end_date = as.POSIXct(
        value_or(station$subscription_end_date, NA),
        origin = "1970-01-01",
        tz = tz
      ),
      time_zone = value_or(station$time_zone, NA),
      city = value_or(station$city, NA),
      region = value_or(station$region, NA),
      country = value_or(station$country, NA),
      latitude = value_or(station$latitude, NA),
      longitude = value_or(station$longitude, NA),
      elevation_m = value_or(station$elevation, NA),
      gateway_type = value_or(station$gateway_type, NA),
      relationship_type = value_or(station$relationship_type, NA),
      subscription_type = value_or(station$subscription_type, NA),
      generated_at = as.POSIXct(
        value_or(response$generated_at, NA),
        origin = "1970-01-01",
        tz = "UTC"
      )
    )
  })
}

#' Get current weather records for a Davis station
#'
#' @param station_uuid Station UUID (or integer station id)
#' @param sensor_types Optional integer vector of sensor types to keep
#'
#' @return Tibble with current weather records
#' @export
get_weather_current_davis <- function(station_uuid, sensor_types = NULL) {
  stopifnot(length(station_uuid) == 1)
  station_uuid <- as.character(station_uuid)

  response <- request_davis_api(
    path = paste0("/current/", station_uuid)
  )

  flatten_davis_weather_records(
    response = response,
    station_uuid = station_uuid,
    sensor_types = sensor_types
  )
}

#' Get historical weather records for a Davis station
#'
#' @param station_uuid Station UUID (or integer station id)
#' @param start_timestamp Unix start timestamp (seconds)
#' @param end_timestamp Unix end timestamp (seconds)
#' @param sensor_types Optional integer vector of sensor types to keep
#'
#' @return Tibble with historical weather records
#' @export
get_weather_historical_davis <- function(
  station_uuid,
  start_timestamp,
  end_timestamp,
  sensor_types = NULL
) {
  stopifnot(length(station_uuid) == 1)
  station_uuid <- as.character(station_uuid)
  stopifnot(is.numeric(start_timestamp), is.numeric(end_timestamp))
  stopifnot(start_timestamp < end_timestamp)
  stopifnot((end_timestamp - start_timestamp) <= 24 * 60 * 60)

  response <- request_davis_api(
    path = paste0("/historic/", station_uuid),
    query = list(
      `start-timestamp` = as.integer(start_timestamp),
      `end-timestamp` = as.integer(end_timestamp)
    )
  )

  flatten_davis_weather_records(
    response = response,
    station_uuid = station_uuid,
    sensor_types = sensor_types
  )
}
