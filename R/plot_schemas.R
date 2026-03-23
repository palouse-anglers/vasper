# Plot Schema Registry ----
# Curated, tested ggplot schema templates.
# Each schema has: name, description, parameters (required/optional with
# types and allowed values), and a template (working R code with
# {{placeholder}} column substitutions).
#
# Templates assume:
#   - `data` is the input data.frame
#   - BRAND_COLORS and palette helpers (`scale_*_vasper_discrete`) are available
#   - ggplot2, dplyr, tidyr, scales, forcats, ggridges, patchwork are loaded

# -- Parameter specification helpers ------------------------------------------

schema_param <- function(
  name,
  type,
  description,
  required = TRUE,
  allowed = NULL,
  default = NULL
) {
  out <- list(
    name = name,
    type = type,
    description = description,
    required = required
  )
  if (!is.null(allowed)) {
    out$allowed <- allowed
  }
  if (!is.null(default)) {
    out$default <- default
  }
  out
}

# -- Schema constructor -------------------------------------------------------

plot_schema <- function(name, description, parameters, template) {
  stopifnot(
    is.character(name),
    nzchar(name),
    is.character(description),
    nzchar(description),
    is.list(parameters),
    is.character(template),
    nzchar(template)
  )
  list(
    name = name,
    description = description,
    parameters = parameters,
    template = template
  )
}

# -- Template substitution ----------------------------------------------------

#' Replace `{{key}}` placeholders in a template string
#'
#' @param template Character string with `{{key}}` tokens.
#' @param values Named list of replacements.
#' @return Substituted string.
substitute_template <- function(template, values) {
  for (nm in names(values)) {
    val <- values[[nm]]

    if (is.null(val) || length(val) == 0) {
      replacement <- ""
    } else if (all(is.na(val))) {
      replacement <- ""
    } else {
      val <- val[!is.na(val)]
      if (length(val) > 1) {
        replacement <- paste(as.character(val), collapse = ", ")
      } else {
        replacement <- as.character(val[[1]])
      }
    }

    if (is.na(replacement) || length(replacement) == 0) {
      replacement <- ""
    }

    template <- gsub(
      paste0("\\{\\{", nm, "\\}\\}"),
      replacement,
      template
    )
  }
  template
}

# -- Schema catalog -----------------------------------------------------------

PLOT_SCHEMAS <- list()

# 1. Basic single-geom chart ---------------------------------------------------

PLOT_SCHEMAS[["basic"]] <- plot_schema(
  name = "basic",
  description = paste(
    "Single-geom chart: scatter, line, bar, boxplot, histogram, or density.",
    "Covers most simple exploratory plots."
  ),
  parameters = list(
    schema_param("x", "column", "Column mapped to x axis"),
    schema_param(
      "y",
      "column",
      "Column mapped to y axis (omit for histogram/density)",
      required = FALSE
    ),
    schema_param(
      "geom",
      "enum",
      "Plot geometry",
      allowed = c("point", "line", "col", "boxplot", "histogram", "density"),
      default = "point"
    ),
    schema_param(
      "color",
      "column",
      "Color/fill grouping column",
      required = FALSE
    ),
    schema_param("facet", "column", "Facet-wrap column", required = FALSE)
  ),
  template = '
mapping <- aes(x = .data[["{{x}}"]])
{{#y}}
mapping$y <- rlang::expr(.data[["{{y}}"]])
{{/y}}
{{#color}}
mapping$colour <- rlang::expr(.data[["{{color}}"]])
mapping$fill   <- rlang::expr(.data[["{{color}}"]])
{{/color}}

p <- ggplot(data, mapping)

p <- p + {{geom_layer}}

{{#color}}
if (is.numeric(data[["{{color}}"]])) {
  p <- p + scale_colour_gradientn(colours = MAP_PALETTE_GREEN)
} else {
  p <- p +
    scale_colour_vasper_discrete(x = data[["{{color}}"]]) +
    scale_fill_vasper_discrete(x = data[["{{color}}"]])
}
{{/color}}

{{#facet}}
p <- p + facet_wrap(~ .data[["{{facet}}"]])
{{/facet}}

p + theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 30, hjust = 1)
  )
'
)

# 2. Grouped boxplot with jitter -----------------------------------------------

PLOT_SCHEMAS[["grouped_boxplot_jitter"]] <- plot_schema(
  name = "grouped_boxplot_jitter",
  description = paste(
    "Boxplots of a measurement by group with jittered raw points and a median",
    "line. Ideal for comparing distributions of a soil metric across producers,",
    "fields, or depths."
  ),
  parameters = list(
    schema_param("x", "column", "Grouping column (e.g. producer_id, depth)"),
    schema_param("y", "column", "Measurement column"),
    schema_param("color", "column", "Color grouping column", required = FALSE)
  ),
  template = '
library(ggplot2)

color_col <- "{{color}}"
use_color <- nzchar(color_col)

mapping <- if (use_color) {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]], colour = .data[[color_col]], fill = .data[[color_col]])
} else {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]])
}

p <- ggplot(data, mapping) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.18, alpha = 0.55, size = 1.6) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5,
               middle.linewidth = 2, colour = BRAND_COLORS$dark)

if (use_color) {
  p <- p +
    scale_colour_vasper_discrete(x = data[[color_col]]) +
    scale_fill_vasper_discrete(x = data[[color_col]])
}

p + theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
'
)

# 3. Faceted trend line --------------------------------------------------------

PLOT_SCHEMAS[["faceted_trend_line"]] <- plot_schema(
  name = "faceted_trend_line",
  description = paste(
    "Year-over-year line trends of a metric faceted by group (field, producer,",
    "depth) with ribbon confidence bands. Best for temporal soil or yield trends."
  ),
  parameters = list(
    schema_param("x", "column", "Temporal x-axis column (e.g. year)"),
    schema_param("y", "column", "Measurement column"),
    schema_param(
      "color",
      "column",
      "Line color grouping column",
      required = FALSE
    ),
    schema_param("facet", "column", "Facet-wrap column (e.g. field_name)")
  ),
  template = '
color_col <- "{{color}}"
use_color <- nzchar(color_col)

mapping <- if (use_color) {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]], colour = .data[[color_col]], fill = .data[[color_col]])
} else {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]])
}

p <- ggplot(data, mapping) +
  geom_ribbon(
    stat = "summary", fun.data = mean_se,
    alpha = 0.18, colour = NA
  ) +
  geom_line(stat = "summary", fun = mean, linewidth = 0.9) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  facet_wrap(~ .data[["{{facet}}"]], scales = "free_y")

if (use_color) {
  p <- p +
    scale_colour_vasper_discrete(x = data[[color_col]]) +
    scale_fill_vasper_discrete(x = data[[color_col]])
}

p + theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text  = element_text(face = "bold")
  )
'
)

# 4. Lollipop with threshold --------------------------------------------------

PLOT_SCHEMAS[["lollipop_threshold"]] <- plot_schema(
  name = "lollipop_threshold",
  description = paste(
    "Lollipop chart of nutrient levels per field/producer with optional",
    "horizontal agronomic threshold reference lines. Great for comparing",
    "measured values against known benchmarks."
  ),
  parameters = list(
    schema_param("x", "column", "Category axis column (e.g. field_name)"),
    schema_param("y", "column", "Measurement column"),
    schema_param("color", "column", "Color grouping column", required = FALSE),
    schema_param(
      "threshold",
      "number",
      "Optional horizontal threshold value for reference line",
      required = FALSE
    ),
    schema_param(
      "threshold_label",
      "string",
      "Label for the threshold line",
      required = FALSE,
      default = "Threshold"
    )
  ),
  template = '
color_col <- "{{color}}"
use_color <- nzchar(color_col)
threshold_val <- "{{threshold}}"
threshold_label <- "{{threshold_label}}"
use_threshold <- nzchar(threshold_val) && !is.na(suppressWarnings(as.numeric(threshold_val)))

mapping <- if (use_color) {
  aes(x = reorder(.data[["{{x}}"]], .data[["{{y}}"]]),
      y = .data[["{{y}}"]],
      colour = .data[[color_col]])
} else {
  aes(x = reorder(.data[["{{x}}"]], .data[["{{y}}"]]),
      y = .data[["{{y}}"]])
}

p <- ggplot(data, mapping) +
  geom_segment(aes(xend = .data[["{{x}}"]], y = 0, yend = .data[["{{y}}"]]),
               linewidth = 0.7, alpha = 0.6) +
  geom_point(size = 3.5, alpha = 0.9)

if (use_threshold) {
  tv <- as.numeric(threshold_val)
  tl <- if (nzchar(threshold_label)) threshold_label else "Threshold"
  p <- p +
    geom_hline(yintercept = tv, linetype = "dashed",
               colour = BRAND_COLORS$danger, linewidth = 0.6) +
    annotate("text", x = Inf, y = tv, label = tl,
             hjust = 1.05, vjust = -0.5, size = 3.2,
             colour = BRAND_COLORS$danger)
}

if (use_color) {
  p <- p + scale_colour_vasper_discrete(x = data[[color_col]])
}

p + coord_flip() +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  )
'
)

# 5. Multi-metric faceted bar --------------------------------------------------

PLOT_SCHEMAS[["multi_metric_facet_bar"]] <- plot_schema(
  name = "multi_metric_facet_bar",
  description = paste(
    "Grouped bar chart comparing 2-4 metrics side-by-side, faceted by group.",
    "The template pivots metric columns to long format automatically.",
    "Ideal for nutrient concentration comparisons by depth or field."
  ),
  parameters = list(
    schema_param(
      "x",
      "column",
      "Category axis column (e.g. depth, field_name)"
    ),
    schema_param(
      "metrics",
      "column_list",
      "2-4 measurement columns to compare side by side"
    ),
    schema_param("facet", "column", "Facet-wrap column", required = FALSE)
  ),
  template = '
metric_cols <- c({{metrics_quoted}})

long <- data |>
  tidyr::pivot_longer(
    cols = all_of(metric_cols),
    names_to = "metric",
    values_to = "value"
  )

p <- ggplot(long, aes(x = .data[["{{x}}"]], y = value,
                       fill = metric)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_vasper_discrete(x = long$metric)

facet_col <- "{{facet}}"
if (nzchar(facet_col)) {
  p <- p + facet_wrap(~ .data[[facet_col]], scales = "free_y")
}

p + theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text  = element_text(face = "bold"),
    legend.position = "bottom"
  )
'
)

# 6. Scatter with marginal densities (patchwork) ------------------------------

PLOT_SCHEMAS[["scatter_with_marginals"]] <- plot_schema(
  name = "scatter_with_marginals",
  description = paste(
    "Scatter plot of two measurements with optional color grouping, plus",
    "marginal density strips on the axes via patchwork. Good for exploring",
    "relationships like OM% vs CEC."
  ),
  parameters = list(
    schema_param("x", "column", "X-axis measurement column"),
    schema_param("y", "column", "Y-axis measurement column"),
    schema_param("color", "column", "Color grouping column", required = FALSE)
  ),
  template = '
color_col <- "{{color}}"
use_color <- nzchar(color_col)

scatter_mapping <- if (use_color) {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]],
      colour = .data[[color_col]])
} else {
  aes(x = .data[["{{x}}"]], y = .data[["{{y}}"]])
}

p_main <- ggplot(data, scatter_mapping) +
  geom_point(alpha = 0.65, size = 2) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

if (use_color) {
  p_main <- p_main +
    scale_colour_vasper_discrete(x = data[[color_col]])
}

# marginal density - top (x)
p_top <- ggplot(data, aes(x = .data[["{{x}}"]])) +
  geom_density(alpha = 0.3, fill = BRAND_COLORS$primary) +
  theme_void() + theme(legend.position = "none")

# marginal density - right (y)
p_right <- ggplot(data, aes(x = .data[["{{y}}"]])) +
  geom_density(alpha = 0.3, fill = BRAND_COLORS$primary) +
  coord_flip() +
  theme_void() + theme(legend.position = "none")

p_top + plot_spacer() + p_main + p_right +
  plot_layout(ncol = 2, nrow = 2,
              widths = c(4, 1), heights = c(1, 4))
'
)

# 7. Stacked proportion bar ---------------------------------------------------

PLOT_SCHEMAS[["stacked_proportion_bar"]] <- plot_schema(
  name = "stacked_proportion_bar",
  description = paste(
    "Proportional stacked bar of categorical or binned measurements across",
    "groups. Shows composition breakdown as percentages."
  ),
  parameters = list(
    schema_param(
      "x",
      "column",
      "Category axis column (e.g. producer_id, year)"
    ),
    schema_param("fill", "column", "Fill/stack grouping column"),
    schema_param("facet", "column", "Optional facet column", required = FALSE)
  ),
  template = '

p <- ggplot(data, aes(x = .data[["{{x}}"]], fill = .data[["{{fill}}"]])) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_vasper_discrete(x = data[["{{fill}}"]])

facet_col <- "{{facet}}"
if (nzchar(facet_col)) {
  p <- p + facet_wrap(~ .data[[facet_col]])
}

p + theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )
'
)

# 8. Ridgeline density --------------------------------------------------------

PLOT_SCHEMAS[["ridgeline_density"]] <- plot_schema(
  name = "ridgeline_density",
  description = paste(
    "Ridge/density plot of a measurement's distribution across groups",
    "(years, producers, depths). Compact distribution comparison."
  ),
  parameters = list(
    schema_param("x", "column", "Measurement column (continuous)"),
    schema_param(
      "y",
      "column",
      "Grouping column (ridge rows, e.g. year, producer_id)"
    )
  ),
  template = '
p <- ggplot(data, aes(x = .data[["{{x}}"]], y = as.factor(.data[["{{y}}"]]),
                       fill = as.factor(.data[["{{y}}"]]))) +
  ggridges::geom_density_ridges(alpha = 0.55, scale = 1.3,
                                 colour = BRAND_COLORS$dark) +
  scale_fill_vasper_discrete(x = as.factor(data[["{{y}}"]])) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(y = NULL)
'
)

# 9. Dual-axis yield + soil ---------------------------------------------------

PLOT_SCHEMAS[["dual_axis_yield_soil"]] <- plot_schema(
  name = "dual_axis_yield_soil",
  description = paste(
    "Paired line chart with one metric on the primary y-axis and another on a",
    "secondary y-axis, sharing x = year. Useful for overlaying yield trends",
    "with soil measurements.",
    "NOTE: y1 and y2 must already be columns in a single pre-joined table;",
    "use query_tables to JOIN yield and soil data first if needed."
  ),
  parameters = list(
    schema_param("x", "column", "Shared x-axis column (e.g. year)"),
    schema_param("y1", "column", "Primary y-axis metric column"),
    schema_param("y2", "column", "Secondary y-axis metric column"),
    schema_param(
      "y1_label",
      "string",
      "Label for primary y-axis",
      required = FALSE,
      default = ""
    ),
    schema_param(
      "y2_label",
      "string",
      "Label for secondary y-axis",
      required = FALSE,
      default = ""
    )
  ),
  template = '
y1_label <- "{{y1_label}}"
y2_label <- "{{y2_label}}"
if (!nzchar(y1_label)) y1_label <- "{{y1}}"
if (!nzchar(y2_label)) y2_label <- "{{y2}}"

range1 <- range(data[["{{y1}}"]], na.rm = TRUE)
range2 <- range(data[["{{y2}}"]], na.rm = TRUE)

scale_factor <- diff(range1) / max(diff(range2), 1e-9)
shift <- range1[1] - range2[1] * scale_factor

p <- ggplot(data, aes(x = .data[["{{x}}"]])) +
  geom_line(aes(y = .data[["{{y1}}"]]),
            colour = BRAND_COLORS$primary, linewidth = 0.9) +
  geom_point(aes(y = .data[["{{y1}}"]]),
             colour = BRAND_COLORS$primary, size = 2) +
  geom_line(aes(y = .data[["{{y2}}"]] * scale_factor + shift),
            colour = BRAND_COLORS$accent, linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = .data[["{{y2}}"]] * scale_factor + shift),
             colour = BRAND_COLORS$accent, size = 2) +
  scale_y_continuous(
    name = y1_label,
    sec.axis = sec_axis(~ (. - shift) / scale_factor, name = y2_label)
  ) +
  theme_minimal() +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.title.y.left  = element_text(colour = BRAND_COLORS$primary),
    axis.title.y.right = element_text(colour = BRAND_COLORS$accent)
  )
'
)

# -- Catalog accessors --------------------------------------------------------

#' Get a schema by name
#'
#' @param name Schema name
#' @return Schema list, or NULL if not found
get_plot_schema <- function(name) {
  PLOT_SCHEMAS[[name]]
}

#' List all registered schema names
#'
#' @return Character vector of schema names
list_plot_schema_names <- function() {
  names(PLOT_SCHEMAS)
}

#' Return the full schema catalog for LLM browsing
#'
#' @return Named list of schemas (each with name, description, parameters, template)
get_plot_schema_catalog <- function() {
  PLOT_SCHEMAS
}
