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
  "CREATE TABLE soil_data AS SELECT * FROM read_csv_auto('data/soil_data.csv')"
)

# Load Data Dictionary into DuckDB
dbExecute(
  con,
  "CREATE TABLE data_dictionary AS SELECT * FROM read_csv_auto('data/data_dictionary.csv')"
)


# Extract Sample Locations and write to DuckDB ----
dbExecute(
  con,
  "
  CREATE TABLE sample_locations AS
  SELECT
    latitude,
    longitude,
    COUNT(*) AS sample_count,
    MIN(sample_date)::VARCHAR AS earliest_date,
    MAX(sample_date)::VARCHAR AS latest_date
  FROM soil_data
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
  GROUP BY latitude, longitude
"
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

# Register Weather Tools with ellmer ----

# Tool: Get Weather Forecast
get_weather_forecast <- tool(
  function(
    latitude,
    longitude,
    n_days = 7,
    variables = eval(as.list(args(get_n_day_forecast))$variables)
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
      tool_name = "forecast",
      param_hash = param_hash
    )
  },
  name = "get_weather_forecast",
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
    variables = type_array(
      type_string(),
      "Open-Meteo daily weather variables. Common options: temperature_2m_max, temperature_2m_min, precipitation_sum, wind_speed_10m_max, soil_temperature_0_to_7cm_mean, soil_moisture_0_to_7cm_mean (note: soil variables NOT available in forecast)",
      required = FALSE
    )
  )
)

# Tool: Get Historical Weather
get_weather_historical <- tool(
  function(
    latitude,
    longitude,
    date_range,
    variables = eval(as.list(args(get_weather_data))$variables)
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
      tool_name = "historical",
      param_hash = param_hash
    )
  },
  name = "get_weather_historical",
  description = "Get historical weather data for a location and date range. Data is written to the database for SQL querying. Returns table metadata with sample rows.",
  arguments = list(
    latitude = type_number(
      "Latitude in decimal degrees (between -90 and 90)"
    ),
    longitude = type_number(
      "Longitude in decimal degrees (between -180 and 180)"
    ),
    date_range = type_array(
      type_string(),
      "Array of two dates in YYYY-MM-DD format: [start_date, end_date]"
    ),
    variables = type_array(
      type_string(),
      "Open-Meteo daily weather variables. Common options: temperature_2m_max, temperature_2m_min, precipitation_sum, wind_speed_10m_max, soil_temperature_0_to_7cm_mean, soil_moisture_0_to_7cm_mean",
      required = FALSE
    )
  )
)

# Initialize Main Chat with Weather Tools ----
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

main_chat <- chat(
  name = chat_provider,
  system_prompt = paste(
    "You are an agricultural advisor assistant for Columbia County.",
    "You have access to weather data tools and soil chemistry data.",
    "\nWeather tools:",
    "- get_weather_forecast: Gets forecast data (up to 16 days)",
    "- get_weather_historical: Gets historical weather data",
    "Both weather tools write data to the same DuckDB database that contains soil_data and sample_locations.",
    "Weather data tables are named weather_forecast_HASH or weather_historical_HASH.",
    "After calling a weather tool, you can query across weather and soil data using SQL.",
    "\nSoil data is in the 'soil_data' table (wide format, one row per sample) with metadata columns:",
    "year, sample_id, producer_id, field_name, field_id, county, longitude, latitude,",
    "sample_date, depth, start_depth_inches, end_depth_inches, huc8_name, hc12_name, huc12",
    "\nand measurement columns (numeric, units in column name):",
    "density_g_ml, density2_mlb_ac, om_percent, ph, ph_ae, ec_mmhos_cm, est_ss_ds_m, effervescence,",
    "cec_meq_100g, est_cec_meq_100g, total_bases_meq_100g, no3_n_mg_kg, no3_n_lb_ac, nh4_n_mg_kg,",
    "nh4_n_lb_ac, p_olsen_mg_kg, bray_p_mg_kg, k_mg_kg, exch_k_meq_100g, ca_mg_kg, mg_mg_kg,",
    "na_mg_kg, s_mg_kg, s_lb_ac, b_mg_kg, cl_mg_kg, cu_mg_kg, fe_mg_kg, mn_mg_kg, zn_mg_kg,",
    "al_mg_kg, al_kcl_mg_kg, ca_kcl_mg_kg, mg_kcl_mg_kg, na_kcl_mg_kg, wilting_point,",
    "field_capacity, h2o_wet_dry, avl_h2o_in, avl_h2o_percent",
    "\nProvide practical agricultural advice based on weather patterns, soil conditions, and farming best practices."
  ),
  echo = "text"
)

# Page Registry ----
# Single source of truth for all navigable pages.
# Add new pages here — hamburger menu, overlay panels, and tools all read from it.
app_pages <- list(
  reports = list(title = "Reports", icon = "file-alt"),
  soil_data = list(title = "Soil Data", icon = "flask")
)

# Register tools
main_chat$register_tool(get_weather_forecast)
main_chat$register_tool(get_weather_historical)

# Cleanup on app stop ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
