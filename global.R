# Global Environment Setup
# Loads packages, data, and initializes AI tools

# UI Framework ----
library(shiny)
library(bslib)
library(shinyWidgets)

# AI & Chat ----
library(ellmer)
library(shinychat)

# Data ----
library(tidyverse)
library(DBI)
library(duckdb)
library(lubridate)
library(digest)

# APIs ----
library(httr2)
library(jsonlite)

# Visualization ----
library(plotly)
library(leaflet)
library(DT)

# Reports ----
library(quarto)
library(zip)

# Async (optional for Phase 2+) ----
# library(promises)
# library(future)

# Load all source code
lapply(
  list.files(path = "R", pattern = "*.R", full.names = TRUE, all.files = TRUE),
  source
)

# Theme ----
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = "#2E7D32", # Agricultural green
  secondary = "#FFA726", # Harvest orange
  font_scale = 0.9
)

# Initialize DuckDB In-Memory Database ----
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Load Soil Data into DuckDB (wide-format CSV produced by data-raw/reformat_soil_data.R)
dbExecute(
  con,
  glue::glue(
    "CREATE TABLE {TABLE_NAMES$soil_data} AS SELECT * FROM read_csv_auto('data/soil_data.csv')"
  )
)

# Load Data Dictionary into DuckDB
dbExecute(
  con,
  glue::glue(
    "CREATE TABLE {TABLE_NAMES$data_dictionary} AS SELECT * FROM read_csv_auto('data/data_dictionary.csv')"
  )
)

# Initialize metadata registry for user-facing data labels
ensure_table_metadata(con)


# Extract Sample Locations and write to DuckDB ----
dbExecute(
  con,
  glue::glue(
    "
  CREATE TABLE {TABLE_NAMES$sample_locations} AS
  SELECT
    latitude,
    longitude,
    COUNT(*) AS sample_count,
    MIN(sample_date)::VARCHAR AS earliest_date,
    MAX(sample_date)::VARCHAR AS latest_date
  FROM {TABLE_NAMES$soil_data}
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
  GROUP BY latitude, longitude
"
  )
)

# Seed metadata labels for startup tables
upsert_table_metadata(
  con = con,
  table_name = TABLE_NAMES$soil_data,
  table_label = "Soil Data",
  source = "startup",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue("SELECT COUNT(*) AS n FROM {TABLE_NAMES$soil_data}")
  )$n[[1]],
  column_count = length(DBI::dbListFields(con, TABLE_NAMES$soil_data)),
  is_active = TRUE
)

upsert_table_metadata(
  con = con,
  table_name = TABLE_NAMES$data_dictionary,
  table_label = "Data Dictionary",
  source = "startup",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue("SELECT COUNT(*) AS n FROM {TABLE_NAMES$data_dictionary}")
  )$n[[1]],
  column_count = length(DBI::dbListFields(con, TABLE_NAMES$data_dictionary)),
  is_active = TRUE
)

upsert_table_metadata(
  con = con,
  table_name = TABLE_NAMES$sample_locations,
  table_label = "Sample Locations",
  source = "startup",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue("SELECT COUNT(*) AS n FROM {TABLE_NAMES$sample_locations}")
  )$n[[1]],
  column_count = length(DBI::dbListFields(con, TABLE_NAMES$sample_locations)),
  is_active = TRUE
)

upsert_table_metadata(
  con = con,
  table_name = TABLE_NAMES$table_metadata,
  table_label = "Table Metadata",
  source = "system",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue("SELECT COUNT(*) AS n FROM {TABLE_NAMES$table_metadata}")
  )$n[[1]],
  column_count = length(DBI::dbListFields(con, TABLE_NAMES$table_metadata)),
  is_active = TRUE
)

# Helper: Hash Tool Parameters ----
hash_tool_params <- function(...) {
  params <- list(...)
  # Sort parameters by name for consistency
  params <- params[order(names(params))]
  # Serialize and hash
  param_string <- paste(names(params), params, sep = "=", collapse = "|")
  substr(digest(param_string, algo = "md5"), 1, 8)
}

normalize_optional_string_array <- function(x, default = NULL) {
  if (is.null(x)) {
    return(default)
  }

  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- toupper(trimws(values))
  values <- values[nzchar(values)]
  values <- unique(values)

  if (length(values) == 0) {
    return(default)
  }

  values
}

normalize_integer_scalar <- function(x, name) {
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(values) != 1) {
    stop(
      paste0("'", name, "' must be a single numeric value."),
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.integer(values[[1]]))
  if (is.na(value)) {
    stop(
      paste0("'", name, "' must be a valid integer year."),
      call. = FALSE
    )
  }

  value
}

tool_icon_image <- function(src, alt = "", bg = NULL) {
  if (
    is.character(src) &&
      length(src) == 1 &&
      nzchar(src) &&
      !grepl("^(https?://|/)", src)
  ) {
    src <- paste0("/", src)
  }

  icon_img <- tags$img(
    src = src,
    alt = alt,
    style = "width:1.3em;height:1.3em;max-width:1.3em;max-height:1.3em;object-fit:contain;vertical-align:text-bottom;"
  )

  if (!is.character(bg) || length(bg) != 1 || !nzchar(bg)) {
    return(icon_img)
  }

  tags$span(
    style = paste0(
      "display:inline-flex;align-items:center;justify-content:center;",
      "width:1.3em;height:1.3em;max-width:1.3em;max-height:1.3em;",
      "background-color:",
      bg,
      ";"
    ),
    icon_img
  )
}

# Register Weather Tools with ellmer ----

# Tool: Get Open-Meteo Forecast
get_weather_forecast_open_meteo <- tool(
  function(
    latitude,
    longitude,
    n_days = 7,
    variables = eval(as.list(args(get_n_day_forecast))$variables),
    table_label,
    add_data_view = TRUE
  ) {
    # Hash parameters for table naming
    param_hash <- hash_tool_params(
      latitude = round(latitude, 4),
      longitude = round(longitude, 4),
      n_days = n_days,
      variables = paste(sort(variables), collapse = ",")
    )

    # Call API function
    weather_data <- get_n_day_forecast(
      latitude = latitude,
      longitude = longitude,
      n = n_days,
      variables = variables
    )

    # Write to database and return metadata
    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "forecast_open_meteo",
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_weather_forecast_open_meteo",
  description = "Get weather forecast data for a location. Data is written to the database for SQL querying. Returns table metadata with sample rows.",
  arguments = list(
    latitude = type_number(
      "Latitude in decimal degrees (between -90 and 90)"
    ),
    longitude = type_number(
      "Longitude in decimal degrees (between -180 and 180)"
    ),
    n_days = type_number(
      "Number of days to forecast (default 7, max 16)",
      required = FALSE
    ),
    table_label = type_string(
      "Required user-facing label for the output table in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add this table as an open Data view (default TRUE)",
      required = FALSE
    ),
    variables = type_array(
      type_string(),
      "Open-Meteo daily weather variables. Common options: temperature_2m_max, temperature_2m_min, precipitation_sum, wind_speed_10m_max, soil_temperature_0_to_7cm_mean, soil_moisture_0_to_7cm_mean (note: soil variables NOT available in forecast)",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Open-Meteo Forecast",
    icon = tool_icon_image("icons/open-meteo-favicon.ico", "Open-Meteo")
  )
)

# Tool: Get Open-Meteo Historical Weather
get_weather_historical_open_meteo <- tool(
  function(
    latitude,
    longitude,
    date_range,
    variables = eval(as.list(args(get_weather_data))$variables),
    table_label,
    add_data_view = TRUE
  ) {
    # Hash parameters for table naming
    param_hash <- hash_tool_params(
      latitude = round(latitude, 4),
      longitude = round(longitude, 4),
      start_date = date_range[1],
      end_date = date_range[2],
      variables = paste(sort(variables), collapse = ",")
    )

    # Call API function
    weather_data <- get_weather_data(
      latitude = latitude,
      longitude = longitude,
      date_range = date_range,
      variables = variables,
      type = "historical"
    )

    # Write to database and return metadata
    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "historical_open_meteo",
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_weather_historical_open_meteo",
  description = "Get historical Open-Meteo weather data for a location and date range, including multi-year periods. Use this tool when historical analysis spans months or many years. Data is written to the database for SQL querying. Returns table metadata with sample rows.",
  arguments = list(
    latitude = type_number(
      "Latitude in decimal degrees (between -90 and 90)"
    ),
    longitude = type_number(
      "Longitude in decimal degrees (between -180 and 180)"
    ),
    date_range = type_array(
      type_string(),
      "Array of two dates in YYYY-MM-DD format: [start_date, end_date]. Multi-year ranges are supported."
    ),
    table_label = type_string(
      "Required user-facing label for the output table in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add this table as an open Data view (default TRUE)",
      required = FALSE
    ),
    variables = type_array(
      type_string(),
      "Open-Meteo daily weather variables. Common options: temperature_2m_max, temperature_2m_min, precipitation_sum, wind_speed_10m_max, soil_temperature_0_to_7cm_mean, soil_moisture_0_to_7cm_mean",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Open-Meteo Historical Weather",
    icon = tool_icon_image("icons/open-meteo-favicon.ico", "Open-Meteo")
  )
)

# Tool: List Davis Weather stations
tool_get_weather_stations_davis <- tool(
  function(
    table_label,
    add_data_view = TRUE
  ) {
    stations <- get_weather_stations_davis()

    param_hash <- hash_tool_params(
      api = "stations_davis"
    )

    write_weather_to_db(
      data = stations,
      con = con,
      tool_name = "stations_davis",
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_weather_stations_davis",
  description = "Get Davis WeatherLink station metadata available to your API key in the app's Columbia County, WA focus area. Use this first when users ask about recent hourly conditions. Writes results to DuckDB and returns table metadata.",
  arguments = list(
    table_label = type_string(
      "Required user-facing label for the output table in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add this table as an open Data view (default TRUE)",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Davis Weather Stations",
    icon = tool_icon_image("icons/weatherlink-logo.png", "WeatherLink")
  )
)

# Tool: Get current Davis weather data
tool_get_weather_current_davis <- tool(
  function(
    station_uuid,
    sensor_types = NULL,
    table_label,
    add_data_view = TRUE
  ) {
    weather_data <- get_weather_current_davis(
      station_uuid = station_uuid,
      sensor_types = sensor_types
    )

    param_hash <- hash_tool_params(
      station_uuid = station_uuid,
      sensor_types = paste(
        sort(if (is.null(sensor_types)) numeric(0) else sensor_types),
        collapse = ","
      )
    )

    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "current_davis",
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_weather_current_davis",
  description = "Get current Davis WeatherLink weather records for one explicit station_uuid. If you do not already have a station_uuid, call get_weather_stations_davis first. Writes results to DuckDB and returns table metadata.",
  arguments = list(
    station_uuid = type_string(
      "Required Davis station UUID (or station ID as a string). Call get_weather_stations_davis first if needed."
    ),
    sensor_types = type_array(
      type_number(),
      "Optional sensor type filter (for example [31])",
      required = FALSE
    ),
    table_label = type_string(
      "Required user-facing label for the output table in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add this table as an open Data view (default TRUE)",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Davis Current Weather",
    icon = tool_icon_image("icons/weatherlink-logo.png", "WeatherLink")
  )
)

# Tool: Get historical Davis weather data
tool_get_weather_historical_davis <- tool(
  function(
    station_uuid,
    start_timestamp,
    end_timestamp,
    sensor_types = NULL,
    table_label,
    add_data_view = TRUE
  ) {
    weather_data <- get_weather_historical_davis(
      station_uuid = station_uuid,
      start_timestamp = start_timestamp,
      end_timestamp = end_timestamp,
      sensor_types = sensor_types
    )

    param_hash <- hash_tool_params(
      station_uuid = station_uuid,
      start_timestamp = as.integer(start_timestamp),
      end_timestamp = as.integer(end_timestamp),
      sensor_types = paste(
        sort(if (is.null(sensor_types)) numeric(0) else sensor_types),
        collapse = ","
      )
    )

    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "historical_davis",
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_weather_historical_davis",
  description = "Get historical Davis WeatherLink weather records for one explicit station_uuid and Unix timestamp window. This endpoint is best for short windows (up to 24 hours per call), not multi-year pulls. If you do not already have a station_uuid, call get_weather_stations_davis first. Writes results to DuckDB and returns table metadata.",
  arguments = list(
    station_uuid = type_string(
      "Required Davis station UUID (or station ID as a string). Call get_weather_stations_davis first if needed."
    ),
    start_timestamp = type_number(
      "Required Unix start timestamp in seconds"
    ),
    end_timestamp = type_number(
      "Required Unix end timestamp in seconds (must be within 24 hours of start_timestamp; use repeated calls for longer history)"
    ),
    sensor_types = type_array(
      type_number(),
      "Optional sensor type filter (for example [31])",
      required = FALSE
    ),
    table_label = type_string(
      "Required user-facing label for the output table in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add this table as an open Data view (default TRUE)",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Davis Historical Weather",
    icon = tool_icon_image("icons/weatherlink-logo.png", "WeatherLink")
  )
)

# Tool: Get USDA NASS historical crop yield data
get_yield_historical_nass <- tool(
  function(
    crops = NULL,
    statistics = c("YIELD", "PRODUCTION", "AREA HARVESTED"),
    year_min = 1980,
    year_max = as.integer(format(Sys.Date(), "%Y")),
    table_label,
    add_data_view = TRUE
  ) {
    crops <- normalize_optional_string_array(crops, default = NULL)
    statistics <- normalize_optional_string_array(
      statistics,
      default = c("YIELD", "PRODUCTION", "AREA HARVESTED")
    )

    year_min <- normalize_integer_scalar(year_min, "year_min")
    year_max <- normalize_integer_scalar(year_max, "year_max")

    if (year_min > year_max) {
      stop(
        "'year_min' must be less than or equal to 'year_max'.",
        call. = FALSE
      )
    }

    param_hash <- hash_tool_params(
      crops = paste(
        sort(if (is.null(crops)) character(0) else crops),
        collapse = ","
      ),
      statistics = paste(sort(statistics), collapse = ","),
      year_min = year_min,
      year_max = year_max
    )

    raw_data <- get_columbia_county_nass_raw(
      crops = crops,
      statistics = statistics,
      year_min = year_min,
      year_max = year_max
    )

    trend_data <- get_columbia_county_nass_trends(raw_data)

    write_yield_to_db(
      raw_data = raw_data,
      trend_data = trend_data,
      con = con,
      param_hash = param_hash,
      table_label = table_label,
      add_data_view = add_data_view
    )
  },
  name = "get_yield_historical_nass",
  description = paste(
    "Get USDA NASS QuickStats historical county-level crop records for Columbia County, WA.",
    "This writes paired DuckDB tables (raw and trend) for SQL analysis.",
    "Use for questions about historical yield/production/harvested area for crops",
    "such as wheat, barley, corn, lentils, and canola."
  ),
  arguments = list(
    crops = type_array(
      type_string(),
      "Optional crop commodity filters (e.g. ['WHEAT', 'BARLEY', 'CORN']). Leave empty for all crops.",
      required = FALSE
    ),
    statistics = type_array(
      type_string(),
      "Statistic categories to request (default: YIELD, PRODUCTION, AREA HARVESTED).",
      required = FALSE
    ),
    year_min = type_number(
      "Minimum year to include (default 1980)",
      required = FALSE
    ),
    year_max = type_number(
      "Maximum year to include (default current year)",
      required = FALSE
    ),
    table_label = type_string(
      "Required user-facing label for the output tables in the Data page"
    ),
    add_data_view = type_boolean(
      "Whether to add output tables as open Data views (default TRUE)",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "USDA NASS",
    icon = tool_icon_image(
      "icons/usda-nass-logo.png",
      "USDA NASS",
      bg = "#09266A"
    )
  )
)

# Prompt Profiles and Chat Tool Registry ----

prompt_config <- resolve_prompt_manifest(file.path("prompts", "manifest.json"))
default_prompt_id <- prompt_config$default_prompt_id

CHAT_TOOLS <- list(
  get_weather_forecast_open_meteo = get_weather_forecast_open_meteo,
  get_weather_historical_open_meteo = get_weather_historical_open_meteo,
  get_weather_stations_davis = tool_get_weather_stations_davis,
  get_weather_current_davis = tool_get_weather_current_davis,
  get_weather_historical_davis = tool_get_weather_historical_davis,
  get_yield_historical_nass = get_yield_historical_nass
)

# Detect available LLM provider based on environment variables
chat_provider <- if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  "anthropic"
} else if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  "openai"
} else {
  stop(
    "No LLM API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY in .Renviron"
  )
}

create_chat_client <- function(system_prompt) {
  chat(
    name = chat_provider,
    system_prompt = system_prompt,
    echo = "all"
  )
}

register_prompt_tools <- function(
  chat_client,
  prompt_profile,
  extra_tools = list()
) {
  selected_tools <- resolve_prompt_tools(prompt_profile, CHAT_TOOLS)
  all_tools <- c(selected_tools, extra_tools)

  if (length(all_tools) > 0) {
    chat_client$register_tools(unname(all_tools))
  }

  invisible(all_tools)
}

# Page Registry ----
# Keep `app_pages` as runtime alias to preserve current call sites.
app_pages <- APP_PAGES

# Cleanup on app stop ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
