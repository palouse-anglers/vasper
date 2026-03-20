# Brand Colors ----
# Parses _brand.yml and exposes BRAND_COLORS for use across R/ files.
# Must be sourced before any file that references BRAND_COLORS (e.g. map_helpers.R).
# Alphabetical load order (brand_colors.R < map_helpers.R) guarantees this.

.brand_raw <- yaml::read_yaml("_brand.yml")
.brand_palette <- .brand_raw$color$palette

.resolve_brand_color <- function(name) {
  val <- .brand_raw$color[[name]]
  if (is.null(val)) {
    return(NULL)
  }
  .brand_palette[[val]] %||% val
}

.resolve_brand_light <- function(name) {
  alias <- .brand_raw$color[[name]]
  if (is.null(alias)) {
    return(NULL)
  }
  .brand_palette[[paste0(alias, "-light")]]
}

BRAND_COLORS <- list(
  primary = .resolve_brand_color("primary"),
  primary_light = .resolve_brand_light("primary"),
  secondary = .resolve_brand_color("secondary"),
  success = .resolve_brand_color("success"),
  warning = .resolve_brand_color("warning"),
  warning_light = .resolve_brand_light("warning"),
  danger = .resolve_brand_color("danger"),
  danger_light = .resolve_brand_light("danger"),
  foreground = .resolve_brand_color("foreground"),
  background = .resolve_brand_color("background"),
  light = .resolve_brand_color("light"),
  dark = .resolve_brand_color("dark"),
  accent = .resolve_brand_color("accent"),
  usda_nass_navy = .brand_palette[["weatherlink-navy"]]
)

rm(.brand_raw, .brand_palette, .resolve_brand_color, .resolve_brand_light)
