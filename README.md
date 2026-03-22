Vasper the friendly VSP reporting tool

## Registered tools (ellmer)

The app registers the following tools in [global.R](global.R):

- `query_tables` — Generic query tables tool
- `get_table_profile` — Deep one-table profile (missingness, distinct counts, sample unique values)
- `get_weather_forecast_open_meteo` — Open-Meteo forecast data (up to 16 days)
- `get_weather_historical_open_meteo` — Open-Meteo historical weather data (including multi-year ranges)
- `get_weather_stations_davis` — Davis WeatherLink station metadata
- `get_weather_current_davis` — Current Davis weather records for a specific `station_uuid`
- `get_weather_historical_davis` — Historical Davis weather records for a specific `station_uuid` and short Unix timestamp window
- `get_yield_historical_nass` — USDA NASS QuickStats historical Columbia County, WA crop data with paired raw and trend tables

## Acknowledgements

Vasper uses the following data tools and services:

- [{soils} R package](https://wa-department-of-agriculture.github.io/soils/)
- [Open-Meteo API](https://open-meteo.com/)
- [Davis Instruments WeatherLink v2 API](https://weatherlink.github.io/v2-api/)
- [USDA NASS QuickStats API](https://quickstats.nass.usda.gov/api)
- {shiny}, {shinychat}, {ellmer}, {duckdb}, and [R packages listed in manifest.json](manifest.json)


The {soils} package was developed by the Washington State Department of
Agriculture and Washington State University as part of the Washington Soil
Health Initiative.

This product uses the NASS API but is not endorsed or certified by NASS.

AI assistance for this project includes GitHub Copilot, ChatGPT (GPT-5.3-Codex), and Claude (Sonnet 4.6).
