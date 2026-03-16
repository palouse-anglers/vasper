# App Configuration ----

# Canonical DuckDB table names used across app layers.
TABLE_NAMES <- list(
  soil_data = "soil_data",
  data_dictionary = "data_dictionary",
  sample_locations = "sample_locations",
  table_metadata = "table_metadata"
)

# Report defaults and conventions.
REPORT_DEFAULTS <- list(
  producer_id = "COLUMBIA CONSERVATION DISTRICT",
  depth_all = "AllDepths"
)

REPORT_FORMATS <- c(
  "HTML" = "html",
  "Word" = "docx",
  "All" = "all"
)

# Navigation and page registry.
APP_PAGE_KEYS <- c("data", "reports")

APP_PAGES <- list(
  data = list(
    title = "Data",
    icon = "database",
    icon_fallback = "table"
  ),
  reports = list(
    title = "Reports",
    icon = "icons/soils-logo.png",
    icon_fallback = "file-alt"
  )
)

# Font Awesome aliasing/fallback rules.
ICON_ALIASES <- c(
  "file-alt" = "file-lines"
)

ICON_FALLBACK <- "file"
