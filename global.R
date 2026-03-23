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
library(thematic)
library(patchwork)
library(ggridges)

# Reports ----
library(quarto)
library(zip)

# Load all source code
# Files in `first` are sourced before the rest.
.r_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
.r_first <- file.path("R", c("brand_colors.R", "app_config.R"))
lapply(c(.r_first, setdiff(.r_files, .r_first)), source)
rm(.r_files, .r_first)

# Suppress bslib dev-mode contrast warnings that originate from the "shiny"
# preset's built-in $blue (#2077d8) being processed through Bootstrap's color
# utility machinery. This is a preset-level quirk, not a brand color issue.
options(bslib.color_contrast_warnings = FALSE)

# Theme ----
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  brand = "_brand.yml",
  font_scale = 0.9
)

# Auto-apply brand colors to all ggplot2/base/lattice plots in the session
thematic_shiny(font = "auto")

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
ensure_visual_artifact_metadata(con)


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
  source_detail = "Loaded at startup data file",
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
  source_detail = "Loaded at startup from data file",
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
  source_detail = "Derived at startup from soil_data grouped by latitude/longitude",
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
  source_detail = "System registry table",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue("SELECT COUNT(*) AS n FROM {TABLE_NAMES$table_metadata}")
  )$n[[1]],
  column_count = length(DBI::dbListFields(con, TABLE_NAMES$table_metadata)),
  is_active = TRUE
)

upsert_table_metadata(
  con = con,
  table_name = TABLE_NAMES$visual_artifact_metadata,
  table_label = "Visualization Artifacts",
  source = "system",
  source_detail = "System visualization artifact registry table",
  row_count = DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT COUNT(*) AS n FROM {TABLE_NAMES$visual_artifact_metadata}"
    )
  )$n[[1]],
  column_count = length(DBI::dbListFields(
    con,
    TABLE_NAMES$visual_artifact_metadata
  )),
  is_active = TRUE
)

# Helper: Deterministic Tool Table Naming ----
normalize_table_name_component <- function(x, default = "na", max_chars = 48L) {
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- trimws(values)
  values <- values[nzchar(values)]

  if (length(values) == 0) {
    return(default)
  }

  value <- paste(values, collapse = "_")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^_|_$", "", value)

  max_chars <- as.integer(max_chars)
  if (!is.na(max_chars) && max_chars > 0L && nchar(value) > max_chars) {
    value <- substr(value, 1, max_chars)
    value <- gsub("_+$", "", value)
  }

  if (!nzchar(value)) {
    return(default)
  }

  value
}

normalize_table_name_values <- function(
  values,
  default = "all",
  max_values = 4L,
  max_chars = 48L
) {
  if (is.null(values)) {
    return(default)
  }

  flat <- unlist(values, recursive = TRUE, use.names = FALSE)
  flat <- as.character(flat)
  flat <- trimws(flat)
  flat <- flat[nzchar(flat)]

  if (length(flat) == 0) {
    return(default)
  }

  flat <- sort(unique(flat))

  max_values <- as.integer(max_values)
  if (is.na(max_values) || max_values < 1L) {
    max_values <- 4L
  }

  if (length(flat) > max_values) {
    flat <- c(
      head(flat, max_values),
      paste0("plus_", length(flat) - max_values)
    )
  }

  normalize_table_name_component(
    flat,
    default = default,
    max_chars = max_chars
  )
}

build_deterministic_scope_name <- function(
  components = list(),
  max_chars = 120L
) {
  if (length(components) == 0) {
    return("default")
  }

  keys <- names(components)
  if (is.null(keys)) {
    keys <- paste0("field", seq_along(components))
  }

  ordering <- order(keys)
  keys <- keys[ordering]
  values <- components[ordering]

  chunks <- vapply(
    seq_along(values),
    function(i) {
      key <- normalize_table_name_component(keys[[i]], default = "field")
      value <- normalize_table_name_values(
        values[[i]],
        default = "all",
        max_values = 4L,
        max_chars = 40L
      )
      paste0(key, "_", value)
    },
    character(1)
  )

  out <- paste(chunks, collapse = "__")

  max_chars <- as.integer(max_chars)
  if (is.na(max_chars) || max_chars < 40L) {
    max_chars <- 120L
  }

  if (nchar(out) > max_chars) {
    out <- substr(out, 1, max_chars)
    out <- gsub("_+$", "", out)
  }

  if (!nzchar(out)) {
    return("default")
  }

  out
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
    scope_name <- build_deterministic_scope_name(list(
      latitude = sprintf("%.4f", round(latitude, 4)),
      longitude = sprintf("%.4f", round(longitude, 4)),
      days = as.integer(n_days)
    ))

    table_name <- paste0("weather__forecast_open_meteo__", scope_name)

    source_detail <- paste0(
      "tool=get_weather_forecast_open_meteo; scope=",
      scope_name
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
      table_name = table_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = source_detail
    )
  },
  name = "get_weather_forecast_open_meteo",
  description = "Get weather forecast data for a location. Data is written to the database for SQL querying. Returns only table metadata (name, label, variable names, dimensions).",
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
    scope_name <- build_deterministic_scope_name(list(
      latitude = sprintf("%.4f", round(latitude, 4)),
      longitude = sprintf("%.4f", round(longitude, 4)),
      start_date = as.character(date_range[1]),
      end_date = as.character(date_range[2])
    ))

    table_name <- paste0("weather__historical_open_meteo__", scope_name)

    source_detail <- paste0(
      "tool=get_weather_historical_open_meteo; scope=",
      scope_name
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
      table_name = table_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = source_detail
    )
  },
  name = "get_weather_historical_open_meteo",
  description = "Get historical Open-Meteo weather data for a location and date range, including multi-year periods. Use this tool when historical analysis spans months or many years. Data is written to the database for SQL querying. Returns only table metadata (name, label, variable names, dimensions).",
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

    table_name <- "weather__stations_davis__scope_all"

    write_weather_to_db(
      data = stations,
      con = con,
      tool_name = "stations_davis",
      table_name = table_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = "tool=get_weather_stations_davis; scope=all_stations"
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

    scope_name <- build_deterministic_scope_name(list(
      station_uuid = station_uuid,
      sensor_types = sort(
        if (is.null(sensor_types)) numeric(0) else sensor_types
      )
    ))

    table_name <- paste0("weather__current_davis__", scope_name)

    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "current_davis",
      table_name = table_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = paste0(
        "tool=get_weather_current_davis; scope=",
        scope_name
      )
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

    scope_name <- build_deterministic_scope_name(list(
      station_uuid = station_uuid,
      start_timestamp = as.integer(start_timestamp),
      end_timestamp = as.integer(end_timestamp),
      sensor_types = sort(
        if (is.null(sensor_types)) numeric(0) else sensor_types
      )
    ))

    table_name <- paste0("weather__historical_davis__", scope_name)

    write_weather_to_db(
      data = weather_data,
      con = con,
      tool_name = "historical_davis",
      table_name = table_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = paste0(
        "tool=get_weather_historical_davis; scope=",
        scope_name
      )
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

    scope_name <- build_deterministic_scope_name(list(
      crops = sort(if (is.null(crops)) character(0) else crops),
      statistics = sort(statistics),
      year_min = year_min,
      year_max = year_max
    ))

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
      table_scope = scope_name,
      table_label = table_label,
      add_data_view = add_data_view,
      source_detail = paste0(
        "tool=get_yield_historical_nass; scope=",
        scope_name
      )
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
    title = "USDA NASS Crop History",
    icon = tool_icon_image(
      "icons/usda-nass-logo.png",
      "USDA NASS",
      # Intentional lockup treatment: USDA NASS mark requires this navy
      # backdrop for legibility/brand consistency. Do not remove.
      bg = BRAND_COLORS$usda_nass_navy
    )
  )
)

# Tool: Query tables with SQL
query_tables <- tool(
  function(
    sql,
    mode = NULL,
    input_tables = NULL,
    output_table_names = NULL,
    output_table_labels = NULL,
    persist = FALSE,
    add_data_view = TRUE,
    max_rows = 200
  ) {
    run_query_tables(
      con = con,
      mode = mode,
      sql = sql,
      input_tables = input_tables,
      output_table_names = output_table_names,
      output_table_labels = output_table_labels,
      persist = isTRUE(persist),
      add_data_view = isTRUE(add_data_view),
      max_rows = as.integer(max_rows)
    )
  },
  name = "query_tables",
  description = paste(
    "Run DuckDB SQL against in-memory tables.",
    "Prefer SQL for most arithmetic, aggregations, and comparisons because results are reviewable and reliable.",
    "If table names are uncertain, call get_table_metadata first.",
    "Use mode='vectorized' to apply one SQL suffix across explicit input tables",
    "(SQL must not contain FROM; it is appended after FROM each input table).",
    "Use mode='free' for full SQL with FROM/JOIN; free mode rejects input_tables.",
    "Destructive SQL statements are blocked.",
    "Persist results when they will be reused for artifacts/evidence; avoid persisting one-off exploration or tiny (<5 row) outputs."
  ),
  arguments = list(
    sql = type_string(
      "DuckDB SQL text. For vectorized mode, provide SQL appended after FROM <table> (e.g., 'WHERE year >= 2020 ORDER BY year'). For free mode, provide full DuckDB SQL."
    ),
    mode = type_enum(
      "Execution mode: vectorized (for input_tables) or free (full SQL with FROM). If omitted, defaults to free when only sql is provided.",
      values = c("vectorized", "free"),
      required = FALSE
    ),
    input_tables = type_array(
      type_string(),
      "Required for vectorized mode. Free mode rejects this argument.",
      required = FALSE
    ),
    output_table_names = type_array(
      type_string(),
      "Output table names. For vectorized mode, length must match input_tables and output_table_labels. For free mode, provide at most one name.",
      required = FALSE
    ),
    output_table_labels = type_array(
      type_string(),
      "Output table labels aligned with output_table_names.",
      required = FALSE
    ),
    persist = type_boolean(
      "Whether to persist query result(s) as table(s). Use TRUE for reusable artifact/evidence tables; keep FALSE for one-off exploration or tiny (<5 row) outputs.",
      required = FALSE
    ),
    add_data_view = type_boolean(
      "When persist=TRUE, whether to queue saved tables for Data view (default TRUE).",
      required = FALSE
    ),
    max_rows = type_number(
      "When persist=FALSE, maximum rows returned in result_rows preview (default 200).",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Query Tables",
    icon = icon("database")
  )
)

# Tool: Deep profile for one table
tool_get_table_profile <- tool(
  function(
    table_name,
    sample_values_n = 3,
    max_sample_chars = 120
  ) {
    get_table_profile(
      con = con,
      table_name = table_name,
      sample_values_n = as.integer(sample_values_n),
      max_sample_chars = as.integer(max_sample_chars)
    )
  },
  name = "get_table_profile",
  description = paste(
    "Deep profile of one table for rapid understanding.",
    "Returns per-column data types, missing/null/blank counts, distinct non-missing counts,",
    "and a small list of unique sample values per column.",
    "Use this when you need detailed diagnostics for a single dataset."
  ),
  arguments = list(
    table_name = type_string("Table name to profile."),
    sample_values_n = type_number(
      "Distinct sample values per column (default 3, max 20).",
      required = FALSE
    ),
    max_sample_chars = type_number(
      "Max characters per sampled value (default 120).",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Table Profile",
    icon = icon("table")
  )
)

# Tool: List plot schema summaries
list_plot_schemas <- tool(
  function() {
    run_list_plot_schemas()
  },
  name = "list_plot_schemas",
  description = paste(
    "Return summary metadata for all available plot schemas.",
    "Includes schema name, description, and parameter spec (no template code).",
    "Call this first, then call read_plot_schemas with selected schema name(s)."
  ),
  arguments = list(),
  annotations = tool_annotations(
    title = "List Plot Schemas",
    icon = icon("list")
  )
)

# Tool: Read selected plot schemas (full payload)
read_plot_schemas <- tool(
  function(schema_names) {
    run_read_plot_schemas(schema_names = schema_names)
  },
  name = "read_plot_schemas",
  description = paste(
    "Return the full payload for selected plot schemas.",
    "Includes template code for the provided schema name(s).",
    "Call list_plot_schemas first, then pass selected schema names here."
  ),
  arguments = list(
    schema_names = type_array(
      type_string(),
      "One or more schema names selected from list_plot_schemas."
    )
  ),
  annotations = tool_annotations(
    title = "Read Plot Schemas",
    icon = icon("book")
  )
)

# Tool: Create plot from schema
create_plot_from_schema <- tool(
  function(
    schema_name,
    table_name,
    column_map,
    description,
    title = NULL,
    subtitle = NULL,
    artifact_name,
    artifact_label = NULL,
    width = 9,
    height = 5,
    dpi = 180,
    add_data_view = TRUE
  ) {
    run_create_plot_from_schema(
      con = con,
      schema_name = schema_name,
      table_name = table_name,
      column_map = column_map,
      description = description,
      title = title,
      subtitle = subtitle,
      artifact_name = artifact_name,
      artifact_label = artifact_label,
      width = width,
      height = height,
      dpi = dpi,
      add_data_view = isTRUE(add_data_view)
    )
  },
  name = "create_plot_from_schema",
  description = paste(
    "Create a ggplot artifact by selecting a named schema and providing column mappings.",
    "Schema templates are curated and tested — prefer this over create_plot_code.",
    "If mapped columns are uncertain, call get_table_profile first and avoid all-missing/high-missing fields.",
    "artifact_name is required.",
    "Call list_plot_schemas first, then read_plot_schemas for selected templates.",
    "Available schemas: basic, grouped_boxplot_jitter, faceted_trend_line,",
    "lollipop_threshold, multi_metric_facet_bar, scatter_with_marginals,",
    "stacked_proportion_bar, ridgeline_density, dual_axis_yield_soil."
  ),
  arguments = list(
    schema_name = type_string(
      "Name of the schema to use (from read_plot_schemas)."
    ),
    table_name = type_string("Input table name in DuckDB."),
    column_map = plot_column_map_type(
      "Named mapping of schema parameters to column names or values."
    ),
    description = type_string(
      "Plain-text description of what the plot shows (required)."
    ),
    title = type_string("Optional chart title.", required = FALSE),
    subtitle = type_string("Optional chart subtitle.", required = FALSE),
    artifact_name = type_string(
      "Required stable artifact id slug.",
      required = TRUE
    ),
    artifact_label = type_string(
      "Optional user-facing artifact label.",
      required = FALSE
    ),
    width = type_number(
      "Output width in inches (default 9).",
      required = FALSE
    ),
    height = type_number(
      "Output height in inches (default 5).",
      required = FALSE
    ),
    dpi = type_number("PNG DPI (default 180).", required = FALSE),
    add_data_view = type_boolean(
      "Whether to add visual_artifact_metadata to Data views (default TRUE).",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Create Plot (Schema)",
    icon = icon("chart-bar")
  )
)

# Tool: Create layered ggplot from code (requires inspiration schemas)
create_plot_code <- tool(
  function(
    plot_code,
    table_names,
    description,
    inspiration_schemas,
    artifact_name,
    artifact_label = NULL,
    title = NULL,
    subtitle = NULL,
    width = 9,
    height = 5,
    dpi = 180,
    limit_rows = 50000,
    add_data_view = TRUE
  ) {
    run_create_plot_code(
      con = con,
      plot_code = plot_code,
      table_names = table_names,
      description = description,
      inspiration_schemas = inspiration_schemas,
      artifact_name = artifact_name,
      artifact_label = artifact_label,
      title = title,
      subtitle = subtitle,
      width = width,
      height = height,
      dpi = dpi,
      limit_rows = as.integer(limit_rows),
      add_data_view = isTRUE(add_data_view)
    )
  },
  name = "create_plot_code",
  description = paste(
    "Create a ggplot artifact from explicit R code.",
    "Use ONLY when no schema template fits. You MUST first call list_plot_schemas/read_plot_schemas,",
    "then pass one or more schema names via inspiration_schemas.",
    "Use get_table_profile first when column quality is uncertain; avoid all-missing/high-missing mappings.",
    "artifact_name is required.",
    "Schema template code is injected as comments for reference.",
    "Tables are available as data frames by name. Code must eval to a ggplot/patchwork object."
  ),
  arguments = list(
    plot_code = type_string(
      "R code that evaluates to a ggplot or patchwork object."
    ),
    table_names = type_array(
      type_string(),
      "One or more DuckDB table names made available to plot_code.",
      required = TRUE
    ),
    description = type_string(
      "Plain-text description of what the plot shows (required)."
    ),
    inspiration_schemas = type_array(
      type_string(),
      "One or more schema names from list_plot_schemas/read_plot_schemas that inspired your code (required).",
      required = TRUE
    ),
    artifact_name = type_string(
      "Required stable artifact id slug.",
      required = TRUE
    ),
    artifact_label = type_string(
      "Optional user-facing artifact label.",
      required = FALSE
    ),
    title = type_string("Optional title override.", required = FALSE),
    subtitle = type_string("Optional subtitle override.", required = FALSE),
    width = type_number(
      "Output width in inches (default 9).",
      required = FALSE
    ),
    height = type_number(
      "Output height in inches (default 5).",
      required = FALSE
    ),
    dpi = type_number("PNG DPI (default 180).", required = FALSE),
    limit_rows = type_number(
      "Maximum rows loaded per input table (default 50000).",
      required = FALSE
    ),
    add_data_view = type_boolean(
      "Whether to add visual_artifact_metadata to Data views (default TRUE).",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Create Plot (Code)",
    icon = icon("code")
  )
)

# Tool: Get visualization artifact metadata
tool_get_visual_artifact_metadata <- tool(
  function(
    artifact_ids = NULL,
    artifact_type = NULL,
    include_inactive = FALSE
  ) {
    get_visual_artifact_metadata(
      con = con,
      artifact_ids = artifact_ids,
      artifact_type = artifact_type,
      include_inactive = isTRUE(include_inactive)
    )
  },
  name = "get_visual_artifact_metadata",
  description = paste(
    "List generated chart/map artifacts from visual_artifact_metadata.",
    "Returns paths to SVG/PNG files plus plotting specs for downstream reuse or editing."
  ),
  arguments = list(
    artifact_ids = type_array(
      type_string(),
      "Optional artifact_id filter.",
      required = FALSE
    ),
    artifact_type = type_string(
      "Optional artifact type filter (e.g. 'plot').",
      required = FALSE
    ),
    include_inactive = type_boolean(
      "Whether to include inactive records (default FALSE).",
      required = FALSE
    )
  ),
  annotations = tool_annotations(
    title = "Artifact Metadata",
    icon = icon("file-image")
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
  get_yield_historical_nass = get_yield_historical_nass,
  query_tables = query_tables,
  get_table_profile = tool_get_table_profile,
  list_plot_schemas = list_plot_schemas,
  read_plot_schemas = read_plot_schemas,
  create_plot_from_schema = create_plot_from_schema,
  create_plot_code = create_plot_code,
  get_visual_artifact_metadata = tool_get_visual_artifact_metadata
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
