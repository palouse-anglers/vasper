# Map Helpers ----
# Brand-consistent colors and utilities for leaflet maps.
# Colors sourced from BRAND_COLORS (parsed from _brand.yml in global.R).

MAP_COLORS <- list(
  primary = BRAND_COLORS$primary,
  secondary = BRAND_COLORS$secondary,
  accent = BRAND_COLORS$accent,
  warning = BRAND_COLORS$warning,
  danger = BRAND_COLORS$danger,
  light = BRAND_COLORS$light,
  bg = BRAND_COLORS$background,
  fg = BRAND_COLORS$foreground
)

# Sequential palette from light green → vasper-green (choropleth / continuous)
MAP_PALETTE_GREEN <- c(
  BRAND_COLORS$primary_light,
  "#a5d6a7",
  "#4caf50",
  BRAND_COLORS$primary
)

# Primary categorical palette for factor/discrete scales
MAP_PALETTE_CATEGORICAL <- c(
  MAP_COLORS$primary,
  MAP_COLORS$accent,
  MAP_COLORS$warning,
  MAP_COLORS$danger,
  MAP_COLORS$secondary
)
