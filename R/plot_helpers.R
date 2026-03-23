# Plot Helpers ----
# Color rules and palette helpers for chart generation.

# Plot palette (discrete)
# Colorblind-friendly first, then interpolate for larger cardinalities.
PLOT_PALETTE_CATEGORICAL <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#CC79A7", # magenta
  "#009E73", # bluish green
  "#D55E00", # vermillion
  "#56B4E9", # sky blue
  "#F0E442", # yellow
  "#999999" # grey
)

PLOT_PALETTE_CONTINUOUS <- c(
  "#F7FBFF",
  "#C6DBEF",
  "#6BAED6",
  "#2171B5",
  "#08306B"
)

PLOT_COLOR_NA <- "#9E9E9E"

plot_palette_discrete <- function(x = NULL, n = NULL) {
  if (is.null(n)) {
    n <- if (is.null(x)) {
      length(PLOT_PALETTE_CATEGORICAL)
    } else {
      length(unique(as.character(stats::na.omit(x))))
    }
  }

  n <- as.integer(n)
  if (is.na(n) || n <= 0) {
    return(character())
  }

  if (n <= length(PLOT_PALETTE_CATEGORICAL)) {
    palette <- PLOT_PALETTE_CATEGORICAL[seq_len(n)]
  } else {
    palette <- grDevices::colorRampPalette(
      PLOT_PALETTE_CATEGORICAL
    )(n)
  }

  if (!is.null(x)) {
    keys <- unique(as.character(stats::na.omit(x)))
    keys <- sort(keys)
    names(palette) <- keys
  }

  palette
}

scale_colour_vasper_discrete <- function(x = NULL, n = NULL, ...) {
  ggplot2::scale_colour_manual(
    values = plot_palette_discrete(x = x, n = n),
    na.value = PLOT_COLOR_NA,
    ...
  )
}

scale_fill_vasper_discrete <- function(x = NULL, n = NULL, ...) {
  ggplot2::scale_fill_manual(
    values = plot_palette_discrete(x = x, n = n),
    na.value = PLOT_COLOR_NA,
    ...
  )
}
