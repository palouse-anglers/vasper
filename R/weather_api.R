# Weather API Wrapper
# Open-Meteo API integration for weather data

# Default Open-Meteo daily variables
# See: https://open-meteo.com/en/docs/historical-weather-api#daily_parameter_definition
default_open_meteo_variables <- c(
  # Daily Parameters
  "temperature_2m_max",
  "temperature_2m_mean",
  "temperature_2m_min",
  "apparent_temperature_max",
  "apparent_temperature_mean",
  "apparent_temperature_min",
  "precipitation_sum",
  "rain_sum",
  "showers_sum",
  "snowfall_sum",
  "precipitation_hours",
  "precipitation_probability_max",
  "precipitation_probability_mean",
  "precipitation_probability_min",
  "weather_code",
  "sunrise",
  "sunset",
  "sunshine_duration",
  "daylight_duration",
  "wind_speed_10m_max",
  "wind_gusts_10m_max",
  "wind_direction_10m_dominant",
  "shortwave_radiation_sum",
  "et0_fao_evapotranspiration",
  "uv_index_max",
  "uv_index_clear_sky_max",
  # Additional daily variables available not documented as daily variables
  "soil_temperature_0_to_7cm_mean",
  "soil_temperature_7_to_28cm_mean",
  "soil_temperature_28_to_100cm_mean",
  "soil_temperature_0_to_100cm_mean",
  "soil_moisture_0_to_7cm_mean",
  "soil_moisture_7_to_28cm_mean",
  "soil_moisture_28_to_100cm_mean",
  "soil_moisture_0_to_100cm_mean"
)

# Additional daily variables available (not included in default)
# - cloud_cover_mean, cloud_cover_max, cloud_cover_min
# - dew_point_2m_mean, dew_point_2m_max, dew_point_2m_min
# - relative_humidity_2m_mean, relative_humidity_2m_max, relative_humidity_2m_min
# - pressure_msl_mean, pressure_msl_max, pressure_msl_min
# - surface_pressure_mean, surface_pressure_max, surface_pressure_min
# - wind_speed_10m_mean, wind_speed_10m_min
# - wind_gusts_10m_mean, wind_gusts_10m_min
# - vapour_pressure_deficit_max
# - snowfall_water_equivalent_sum
# - wet_bulb_temperature_2m_mean, wet_bulb_temperature_2m_max, wet_bulb_temperature_2m_min

#' Get Weather Data from Open-Meteo API
#'
#' @param latitude Numeric latitude in decimal degrees
#' @param longitude Numeric longitude in decimal degrees
#' @param date_range Character vector of length 2: c(start_date, end_date) in "YYYY-MM-DD" format
#' @param variables Character vector of Open-Meteo API daily variable names
#'   (e.g., "temperature_2m_max", "precipitation_sum", "wind_speed_10m_max")
#' @param type Character: "historical" or "forecast"
#'
#' @return Tibble with weather data
#' @export
get_weather_data <- function(
  latitude,
  longitude,
  date_range,
  variables = default_open_meteo_variables,
  type = "forecast"
) {
  # Validate inputs
  stopifnot(
    is.numeric(latitude),
    is.numeric(longitude),
    latitude >= -90,
    latitude <= 90,
    longitude >= -180,
    longitude <= 180,
    type %in% c("historical", "forecast")
  )

  # Base URL
  base_url <- if (type == "forecast") {
    "https://api.open-meteo.com/v1/forecast"
  } else {
    "https://archive-api.open-meteo.com/v1/archive"
  }

  # Use API variable names directly
  api_vars <- unique(variables)

  # Build request - different parameters for forecast vs historical
  if (type == "forecast") {
    # Forecast API uses forecast_days and past_days parameters
    # forecast_days starts from tomorrow, past_days=1 brings in yesterday
    # Request one extra day since we filter to the exact date range anyway
    days_diff <- as.numeric(difftime(
      as.Date(date_range[2]),
      as.Date(date_range[1]),
      units = "days"
    ))

    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        latitude = latitude,
        longitude = longitude,
        forecast_days = days_diff + 1, # Request extra day for filtering
        past_days = 1, # Includes recent past data
        daily = paste(api_vars, collapse = ","),
        temperature_unit = "fahrenheit",
        wind_speed_unit = "mph",
        precipitation_unit = "inch",
        timezone = "America/Los_Angeles"
      )
  } else {
    # Historical API uses start_date and end_date
    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        latitude = latitude,
        longitude = longitude,
        start_date = date_range[1],
        end_date = date_range[2],
        daily = paste(api_vars, collapse = ","),
        temperature_unit = "fahrenheit",
        wind_speed_unit = "mph",
        precipitation_unit = "inch",
        timezone = "America/Los_Angeles"
      )
  }

  # Execute request
  resp <- req |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  # Parse response
  data <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE)

  # Convert to tibble
  result <- tibble::as_tibble(data$daily) |>
    dplyr::mutate(
      time = as.Date(time),
      latitude = latitude,
      longitude = longitude
    ) |>
    # Filter to requested date range (important for forecast API with past_days)
    dplyr::filter(
      time >= as.Date(date_range[1]),
      time <= as.Date(date_range[2])
    )

  # Calculate frost days if temperature_2m_min is present
  # A frost day is when the minimum temperature is <= 32°F
  if ("temperature_2m_min" %in% names(result)) {
    result <- result |>
      dplyr::mutate(frost_day = temperature_2m_min <= 32)
  }

  return(result)
}

#' Get N-Day Forecast
#'
#' @param latitude Numeric latitude
#' @param longitude Numeric longitude
#' @param n Number of days to forecast (default 7, max 16)
#' @param variables Character vector of Open-Meteo API daily variable names
#'
#' @return Tibble with n-day forecast
#' @export
get_n_day_forecast <- function(
  latitude,
  longitude,
  n = 7,
  variables = default_open_meteo_variables
) {
  # Validate n
  stopifnot(n > 0, n <= 16)

  # For forecast API, we want n days starting from today
  # The date_range will be used to calculate forecast_days in get_weather_data
  today <- Sys.Date()
  # We pass n days from today, but get_weather_data will use forecast_days parameter
  date_range <- c(as.character(today), as.character(today + n - 1))

  get_weather_data(
    latitude = latitude,
    longitude = longitude,
    date_range = date_range,
    variables = variables,
    type = "forecast"
  )
}

#' Write Weather Data to Database
#'
#' @param data Tibble with weather data
#' @param con Database connection
#' @param tool_name Name of the tool (e.g., "forecast", "historical")
#' @param table_name Deterministic output table name
#' @param table_label User-facing label for the table metadata registry
#' @param add_data_view Whether this table should be added to Data views
#' @param source_detail Optional metadata source detail text
#' @param overwrite Whether to overwrite existing table data
#'
#' @return List with table metadata only
#' @export
write_weather_to_db <- function(
  data,
  con,
  tool_name,
  table_name,
  table_label,
  add_data_view = TRUE,
  source_detail = NULL,
  overwrite = TRUE
) {
  normalize_add_data_view <- function(x, default = TRUE) {
    if (is.logical(x) && length(x) == 1 && !is.na(x)) {
      return(x)
    }

    if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
      return(x != 0)
    }

    if (is.character(x) && length(x) == 1 && nzchar(x)) {
      value <- tolower(trimws(x))
      if (value %in% c("true", "t", "1", "yes", "y")) {
        return(TRUE)
      }
      if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
    }

    default
  }

  add_data_view_flag <- normalize_add_data_view(add_data_view, default = TRUE)
  overwrite_flag <- isTRUE(overwrite)

  table_name <- trimws(as.character(table_name[[1]]))
  if (!nzchar(table_name)) {
    stop("'table_name' must be a non-empty string.", call. = FALSE)
  }

  max_table_name_chars <- 128L
  if (nchar(table_name) > max_table_name_chars) {
    table_name <- substr(table_name, 1, max_table_name_chars)
    table_name <- gsub("_+$", "", table_name)
  }

  DBI::dbWriteTable(con, table_name, data, overwrite = overwrite_flag)

  # Read back for metadata
  row_count <- nrow(data)
  columns <- names(data)
  source_detail <- source_detail %||%
    paste0(
      "tool=",
      paste0("get_weather_", tool_name),
      "; table_name=",
      table_name,
      "; overwrite=",
      overwrite_flag
    )

  # Upsert label metadata (required for Data page selectors)
  upsert_table_metadata(
    con = con,
    table_name = table_name,
    table_label = table_label,
    source = paste0("weather_", tool_name),
    source_detail = source_detail,
    row_count = row_count,
    column_count = length(columns),
    is_active = TRUE
  )

  # Return metadata as JSON-serializable list
  list(
    table_name = table_name,
    table_label = table_label,
    add_data_view = isTRUE(add_data_view_flag),
    variable_names = columns,
    dimensions = list(
      nrow = as.integer(row_count),
      ncol = as.integer(length(columns))
    )
  )
}
