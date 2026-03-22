# visualization_tools & plot_schemas tests
#
# Run from project root with:
#   "C:\Program Files\R\R-4.4.2\bin\x64\Rscript.exe" tests/test_visualization_tools.R

library(testthat)
library(DBI)
library(duckdb)
library(tibble)
library(ggplot2)
library(patchwork)
library(ggridges)

TABLE_NAMES <- list(
  visual_artifact_metadata = "visual_artifact_metadata"
)

source(file.path("R", "brand_colors.R"))
source(file.path("R", "map_helpers.R"))
source(file.path("R", "plot_schemas.R"))
source(file.path("R", "visualization_tools.R"))

# -- Helper: create a fresh DuckDB with sample soil-like data -----------------

make_test_db <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWriteTable(
    con,
    "soil_samples",
    tibble(
      year = rep(2020:2023, each = 6),
      producer_id = rep(c("A", "B", "C"), times = 8),
      field_name = rep(c("North", "South"), times = 12),
      depth = rep(c("0-6", "6-12"), each = 3, times = 4),
      organic_matter = runif(24, 1.5, 6.5),
      cec = runif(24, 8, 30),
      ph = runif(24, 5.5, 8.0),
      nitrogen = runif(24, 10, 80),
      phosphorus = runif(24, 5, 60),
      potassium = runif(24, 80, 300),
      yield = runif(24, 30, 90)
    ),
    overwrite = TRUE
  )

  con
}

# =============================================================================
# 1. Schema registry
# =============================================================================

describe("plot schema registry", {
  test_that("list_plot_schema_names returns all 9 schemas", {
    nms <- list_plot_schema_names()
    expect_true(length(nms) >= 9)
    expect_true(all(
      c(
        "basic",
        "grouped_boxplot_jitter",
        "faceted_trend_line",
        "lollipop_threshold",
        "multi_metric_facet_bar",
        "scatter_with_marginals",
        "stacked_proportion_bar",
        "ridgeline_density",
        "dual_axis_yield_soil"
      ) %in%
        nms
    ))
  })

  test_that("get_plot_schema returns valid schema or NULL", {
    s <- get_plot_schema("basic")
    expect_true(is.list(s))
    expect_equal(s$name, "basic")
    expect_true(nzchar(s$template))
    expect_true(length(s$parameters) > 0)

    expect_null(get_plot_schema("nonexistent_schema"))
  })

  test_that("get_plot_schema_catalog returns all schemas", {
    catalog <- get_plot_schema_catalog()
    expect_true(is.list(catalog))
    expect_true(length(catalog) >= 9)
    expect_true(all(names(catalog) == list_plot_schema_names()))
  })
})

# =============================================================================
# 2. substitute_template
# =============================================================================

describe("substitute_template", {
  test_that("basic replacement works", {
    result <- substitute_template(
      "Hello {{name}}, age {{age}}",
      list(name = "Alice", age = "30")
    )
    expect_equal(result, "Hello Alice, age 30")
  })

  test_that("missing keys are left as-is", {
    result <- substitute_template("x={{x}}, y={{y}}", list(x = "1"))
    expect_equal(result, "x=1, y={{y}}")
  })
})

# =============================================================================
# 3. resolve_basic_geom_layer
# =============================================================================

describe("resolve_basic_geom_layer", {
  test_that("all valid geoms resolve", {
    for (g in c("point", "line", "col", "boxplot", "histogram", "density")) {
      layer <- resolve_basic_geom_layer(g)
      expect_true(grepl(paste0("geom_", g), layer) || grepl("geom_", layer))
    }
  })

  test_that("unknown geom throws error", {
    expect_error(resolve_basic_geom_layer("pie"), "Unsupported geom")
  })
})

# =============================================================================
# 4. build_code_from_schema – each template produces parseable R code
# =============================================================================

describe("build_code_from_schema templates produce parseable R code", {
  con <- make_test_db()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  test_that("basic schema code parses", {
    s <- get_plot_schema("basic")
    code <- build_code_from_schema(
      s,
      list(x = "producer_id", y = "organic_matter", geom = "point")
    )
    expect_silent(parse(text = code))
  })

  test_that("grouped_boxplot_jitter code parses", {
    s <- get_plot_schema("grouped_boxplot_jitter")
    code <- build_code_from_schema(
      s,
      list(x = "producer_id", y = "organic_matter")
    )
    expect_silent(parse(text = code))
  })

  test_that("faceted_trend_line code parses", {
    s <- get_plot_schema("faceted_trend_line")
    code <- build_code_from_schema(
      s,
      list(x = "year", y = "organic_matter", facet = "field_name")
    )
    expect_silent(parse(text = code))
  })

  test_that("lollipop_threshold code parses", {
    s <- get_plot_schema("lollipop_threshold")
    code <- build_code_from_schema(s, list(x = "field_name", y = "ph"))
    expect_silent(parse(text = code))
  })

  test_that("multi_metric_facet_bar code parses", {
    s <- get_plot_schema("multi_metric_facet_bar")
    code <- build_code_from_schema(
      s,
      list(x = "depth", metrics = c("nitrogen", "phosphorus"))
    )
    expect_silent(parse(text = code))
  })

  test_that("scatter_with_marginals code parses", {
    s <- get_plot_schema("scatter_with_marginals")
    code <- build_code_from_schema(s, list(x = "organic_matter", y = "cec"))
    expect_silent(parse(text = code))
  })

  test_that("stacked_proportion_bar code parses", {
    s <- get_plot_schema("stacked_proportion_bar")
    code <- build_code_from_schema(s, list(x = "year", fill = "depth"))
    expect_silent(parse(text = code))
  })

  test_that("ridgeline_density code parses", {
    s <- get_plot_schema("ridgeline_density")
    code <- build_code_from_schema(s, list(x = "organic_matter", y = "year"))
    expect_silent(parse(text = code))
  })

  test_that("dual_axis_yield_soil code parses", {
    s <- get_plot_schema("dual_axis_yield_soil")
    code <- build_code_from_schema(
      s,
      list(x = "year", y1 = "yield", y2 = "organic_matter")
    )
    expect_silent(parse(text = code))
  })
})

# =============================================================================
# 5. build_code_from_schema – conditional blocks
# =============================================================================

describe("build_code_from_schema conditional block handling", {
  test_that("basic schema includes color block when color provided", {
    s <- get_plot_schema("basic")
    code <- build_code_from_schema(
      s,
      list(
        x = "producer_id",
        y = "organic_matter",
        geom = "point",
        color = "field_name"
      )
    )
    expect_true(grepl("scale_colour_manual", code, fixed = TRUE))
  })

  test_that("basic schema omits color block when color absent", {
    s <- get_plot_schema("basic")
    code <- build_code_from_schema(
      s,
      list(x = "producer_id", y = "organic_matter", geom = "point")
    )
    expect_false(grepl("scale_colour_manual", code, fixed = TRUE))
  })

  test_that("title and subtitle are appended via labs()", {
    s <- get_plot_schema("basic")
    code <- build_code_from_schema(
      s,
      list(x = "producer_id", y = "organic_matter", geom = "col"),
      overrides = list(title = "My Plot", subtitle = "Sub")
    )
    expect_true(grepl('title = "My Plot"', code, fixed = TRUE))
    expect_true(grepl('subtitle = "Sub"', code, fixed = TRUE))
  })
})

# =============================================================================
# 6. run_create_plot_from_schema – end to end
# =============================================================================

describe("run_create_plot_from_schema", {
  test_that("schema plot writes under configurable artifact root", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    custom_root <- tempfile("vasper-artifacts-")
    withr::local_options(list(vasper.artifact_dir = custom_root))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(x = "producer_id", y = "organic_matter", geom = "col"),
      description = "Bar chart of OM by producer",
      artifact_name = "test_schema_custom_root"
    )

    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))

    root_norm <- normalizePath(custom_root, winslash = "/", mustWork = FALSE)
    png_norm <- normalizePath(result$files$png, winslash = "/", mustWork = TRUE)
    expect_true(startsWith(png_norm, root_norm))
  })

  test_that("basic schema creates artifact files and metadata", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(x = "producer_id", y = "organic_matter", geom = "col"),
      description = "Bar chart of OM by producer",
      artifact_name = "test_basic_schema"
    )

    expect_equal(result$artifact_type, "plot")
    expect_equal(result$artifact_id, "test_basic_schema")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$mode, "schema")
    expect_equal(result$spec$schema_name, "basic")
  })

  test_that("unknown schema throws informative error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "does_not_exist",
        table_name = "soil_samples",
        column_map = list(x = "year"),
        description = "test",
        artifact_name = "test_unknown_schema"
      ),
      "Unknown schema"
    )
  })

  test_that("missing required column_map params error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "basic",
        table_name = "soil_samples",
        column_map = list(),
        description = "test",
        artifact_name = "test_missing_required_params"
      ),
      "Missing required.*Received keys"
    )
  })

  test_that("missing artifact_name throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "basic",
        table_name = "soil_samples",
        column_map = list(x = "year", y = "ph", geom = "point"),
        description = "test",
        artifact_name = NULL
      ),
      "artifact_name.*required"
    )
  })

  test_that("column_map JSON object string is parsed correctly", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = '{"x":"ph","geom":"histogram"}',
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_json_map"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map wrapped under column_map key is parsed correctly", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(column_map = list(x = "ph", geom = "histogram")),
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_wrapped_map"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map wrapped as JSON string under column_map key is parsed", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(column_map = '{"x":"ph","geom":"histogram"}'),
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_wrapped_json_map"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map keys are normalized to lowercase", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(X = "ph", GEOM = "histogram"),
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_uppercase_keys"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map single-item list containing JSON is parsed", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list('{"x":"ph","geom":"histogram"}'),
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_list_json_map"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map empty-name wrapper with JSON payload is parsed", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    wrapped <- stats::setNames(
      list('{"x":"ph","geom":"histogram"}'),
      ""
    )

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = wrapped,
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_blank_name_json_map"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("column_map name/value pair array is parsed", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(
        list(name = "x", value = "ph"),
        list(name = "geom", value = "histogram")
      ),
      description = "Histogram of pH",
      artifact_name = "test_basic_schema_name_value_array"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "ph")
    expect_equal(result$spec$column_map$geom, "histogram")
  })

  test_that("invalid column_map type throws informative error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "basic",
        table_name = "soil_samples",
        column_map = c("x", "ph"),
        description = "test",
        artifact_name = "test_invalid_column_map"
      ),
      "column_map.*named object/list"
    )
  })

  test_that("missing description throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "basic",
        table_name = "soil_samples",
        column_map = list(x = "year", y = "ph", geom = "point"),
        description = NULL,
        artifact_name = "test_missing_description_schema"
      ),
      "description.*required"
    )
  })

  test_that("nonexistent table throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      run_create_plot_from_schema(
        con = con,
        schema_name = "basic",
        table_name = "no_such_table",
        column_map = list(x = "a", y = "b", geom = "point"),
        description = "test",
        artifact_name = "test_missing_table"
      ),
      "not found|does not exist"
    )
  })

  # -- Advanced schema E2E tests ----------------------------------------------

  test_that("grouped_boxplot_jitter creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "grouped_boxplot_jitter",
      table_name = "soil_samples",
      column_map = list(x = "producer_id", y = "organic_matter"),
      description = "OM distribution by producer",
      artifact_name = "test_grouped_boxplot"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "grouped_boxplot_jitter")
  })

  test_that("schema call tolerates NA/empty optional keys in column_map", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "grouped_boxplot_jitter",
      table_name = "soil_samples",
      column_map = list(
        x = "depth",
        y = "ph",
        geom = NA_character_,
        color = NA_character_,
        facet = NA_character_,
        fill = NA_character_,
        metrics = character(0),
        y1 = NA_character_,
        y2 = NA_character_,
        threshold = NA_real_
      ),
      description = "Boxplot showing pH by depth",
      artifact_name = "test_grouped_boxplot_na_payload"
    )

    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$column_map$x, "depth")
    expect_equal(result$spec$column_map$y, "ph")
    expect_false("geom" %in% names(result$spec$column_map))
  })

  test_that("faceted_trend_line creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "faceted_trend_line",
      table_name = "soil_samples",
      column_map = list(x = "year", y = "organic_matter", facet = "field_name"),
      description = "OM trends by field",
      artifact_name = "test_faceted_trend"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "faceted_trend_line")
  })

  test_that("lollipop_threshold creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "lollipop_threshold",
      table_name = "soil_samples",
      column_map = list(x = "field_name", y = "ph"),
      description = "pH by field",
      artifact_name = "test_lollipop"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "lollipop_threshold")
  })

  test_that("multi_metric_facet_bar creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "multi_metric_facet_bar",
      table_name = "soil_samples",
      column_map = list(x = "depth", metrics = c("nitrogen", "phosphorus")),
      description = "Nitrogen and phosphorus by depth",
      artifact_name = "test_multi_metric"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "multi_metric_facet_bar")
  })

  test_that("scatter_with_marginals creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "scatter_with_marginals",
      table_name = "soil_samples",
      column_map = list(x = "organic_matter", y = "cec"),
      description = "OM vs CEC scatter with marginals",
      artifact_name = "test_scatter_marginals"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "scatter_with_marginals")
  })

  test_that("stacked_proportion_bar creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "stacked_proportion_bar",
      table_name = "soil_samples",
      column_map = list(x = "year", fill = "depth"),
      description = "Depth composition by year",
      artifact_name = "test_stacked_prop"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "stacked_proportion_bar")
  })

  test_that("ridgeline_density creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "ridgeline_density",
      table_name = "soil_samples",
      column_map = list(x = "organic_matter", y = "year"),
      description = "OM distribution by year",
      artifact_name = "test_ridgeline"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "ridgeline_density")
  })

  test_that("dual_axis_yield_soil creates artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
    result <- run_create_plot_from_schema(
      con = con,
      schema_name = "dual_axis_yield_soil",
      table_name = "soil_samples",
      column_map = list(x = "year", y1 = "yield", y2 = "organic_matter"),
      description = "Yield vs OM over time",
      artifact_name = "test_dual_axis"
    )
    expect_equal(result$artifact_type, "plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$schema_name, "dual_axis_yield_soil")
  })
})

# =============================================================================
# 7. run_create_plot_code – inspiration gate
# =============================================================================

describe("run_create_plot_code", {
  test_that("code plot writes under configurable artifact root", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    custom_root <- tempfile("vasper-artifacts-")
    withr::local_options(list(vasper.artifact_dir = custom_root))

    code <- paste(
      "ggplot2::ggplot(soil_samples, ggplot2::aes(x = producer_id, y = organic_matter)) +",
      "  ggplot2::geom_col(fill = BRAND_COLORS$primary, alpha = 0.8) +",
      "  ggplot2::theme_minimal()"
    )

    result <- run_create_plot_code(
      con = con,
      plot_code = code,
      table_names = "soil_samples",
      description = "Simple bar chart of OM by producer",
      inspiration_schemas = c("basic"),
      artifact_name = "test_code_custom_root"
    )

    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))

    root_norm <- normalizePath(custom_root, winslash = "/", mustWork = FALSE)
    png_norm <- normalizePath(result$files$png, winslash = "/", mustWork = TRUE)
    expect_true(startsWith(png_norm, root_norm))
  })

  test_that("missing inspiration_schemas throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- paste(
      "ggplot2::ggplot(data, ggplot2::aes(x = producer_id, y = organic_matter)) +",
      "  ggplot2::geom_col()"
    )

    expect_error(
      run_create_plot_code(
        con = con,
        plot_code = code,
        table_names = "soil_samples",
        description = "Bar chart",
        inspiration_schemas = character(),
        artifact_name = "test_missing_inspiration"
      ),
      "inspiration_schemas.*required"
    )
  })

  test_that("unknown inspiration schema throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- paste(
      "ggplot2::ggplot(data, ggplot2::aes(x = producer_id, y = organic_matter)) +",
      "  ggplot2::geom_col()"
    )

    expect_error(
      run_create_plot_code(
        con = con,
        plot_code = code,
        table_names = "soil_samples",
        description = "Bar chart",
        inspiration_schemas = c("fake_schema"),
        artifact_name = "test_unknown_inspiration"
      ),
      "Unknown inspiration"
    )
  })

  test_that("missing artifact_name throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- paste(
      "ggplot2::ggplot(soil_samples, ggplot2::aes(x = producer_id, y = organic_matter)) +",
      "  ggplot2::geom_col()"
    )

    expect_error(
      run_create_plot_code(
        con = con,
        plot_code = code,
        table_names = "soil_samples",
        description = "Bar chart",
        inspiration_schemas = c("basic"),
        artifact_name = NULL
      ),
      "artifact_name.*required"
    )
  })

  test_that("valid code with inspiration saves artifact", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- paste(
      "ggplot2::ggplot(soil_samples, ggplot2::aes(x = producer_id, y = organic_matter)) +",
      "  ggplot2::geom_col(fill = BRAND_COLORS$primary, alpha = 0.8) +",
      "  ggplot2::theme_minimal()"
    )

    result <- run_create_plot_code(
      con = con,
      plot_code = code,
      table_names = "soil_samples",
      description = "Simple bar chart of OM by producer",
      inspiration_schemas = c("basic"),
      artifact_name = "test_code_plot"
    )

    expect_equal(result$artifact_type, "plot")
    expect_equal(result$artifact_id, "test_code_plot")
    expect_true(file.exists(result$files$svg))
    expect_true(file.exists(result$files$png))
    expect_equal(result$spec$mode, "code")
    expect_true(nzchar(result$spec$plot_code))
  })

  test_that("missing description throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- "ggplot2::ggplot(data, ggplot2::aes(x = year)) + ggplot2::geom_bar()"

    expect_error(
      run_create_plot_code(
        con = con,
        plot_code = code,
        table_names = "soil_samples",
        description = NULL,
        inspiration_schemas = c("basic"),
        artifact_name = "test_missing_description"
      ),
      "description.*required"
    )
  })
})

# =============================================================================
# 8. get_visual_artifact_metadata
# =============================================================================

describe("get_visual_artifact_metadata", {
  test_that("returns structured artifact with description", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(x = "producer_id", y = "cec", geom = "boxplot"),
      description = "CEC by producer",
      artifact_name = "meta_probe"
    )

    out <- get_visual_artifact_metadata(
      con = con,
      artifact_ids = c("meta_probe")
    )

    expect_true("artifacts" %in% names(out))
    expect_equal(length(out$artifacts), 1)
    expect_equal(out$artifacts[[1]]$artifact_id, "meta_probe")
    expect_equal(out$artifacts[[1]]$description, "CEC by producer")
    expect_equal(out$artifacts[[1]]$spec$schema_name, "basic")
  })
})

# =============================================================================
# 9. rerender_visual_artifact
# =============================================================================

describe("rerender_visual_artifact", {
  test_that("rerender schema artifact with overrides", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    run_create_plot_from_schema(
      con = con,
      schema_name = "basic",
      table_name = "soil_samples",
      column_map = list(x = "depth", y = "nitrogen", geom = "boxplot"),
      description = "Nitrogen by depth",
      artifact_name = "rerender_schema"
    )

    rerender_visual_artifact(
      con = con,
      artifact_id = "rerender_schema",
      overrides = list(title = "Updated Title")
    )

    out <- get_visual_artifact_metadata(
      con = con,
      artifact_ids = c("rerender_schema")
    )

    expect_equal(out$artifacts[[1]]$spec$title, "Updated Title")
  })

  test_that("rerender code artifact with overrides", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    code <- paste(
      "ggplot2::ggplot(soil_samples, ggplot2::aes(x = year, y = yield)) +",
      "  ggplot2::geom_line() +",
      "  ggplot2::theme_minimal()"
    )

    run_create_plot_code(
      con = con,
      plot_code = code,
      table_names = "soil_samples",
      description = "Yield over time",
      inspiration_schemas = c("basic"),
      artifact_name = "rerender_code"
    )

    rerender_visual_artifact(
      con = con,
      artifact_id = "rerender_code",
      overrides = list(title = "Code Plot Rerendered")
    )

    out <- get_visual_artifact_metadata(
      con = con,
      artifact_ids = c("rerender_code")
    )

    expect_equal(out$artifacts[[1]]$spec$title, "Code Plot Rerendered")
  })

  test_that("rerender unknown artifact throws error", {
    con <- make_test_db()
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

    expect_error(
      rerender_visual_artifact(con = con, artifact_id = "no_such_artifact"),
      "Unknown artifact_id"
    )
  })
})

# =============================================================================
# 10. schema list/read tool handlers
# =============================================================================

describe("schema list/read tool handlers", {
  test_that("run_list_plot_schemas returns summary catalog", {
    out <- run_list_plot_schemas()
    expect_true(is.list(out))
    expect_true(length(out) > 0)
    expect_true(all(vapply(out, function(x) !is.null(x$name), logical(1))))
    expect_true(all(vapply(out, function(x) is.null(x$template), logical(1))))
  })

  test_that("run_read_plot_schemas returns full payload for selected schemas", {
    out <- run_read_plot_schemas(
      schema_names = c("basic", "faceted_trend_line")
    )
    expect_true(is.list(out))
    expect_equal(length(out), 2)
    expect_true(all(vapply(out, function(x) nzchar(x$template), logical(1))))
  })

  test_that("run_read_plot_schemas errors on empty selection", {
    expect_error(
      run_read_plot_schemas(schema_names = character()),
      "schema_names.*at least one"
    )
  })
})

# =============================================================================
# 11. ellmer tool argument schema regression
# =============================================================================

describe("ellmer object argument schemas", {
  test_that("optional free-form object schema validates", {
    skip_if_not_installed("ellmer")

    schema <- ellmer::type_object(
      .description = "Spec overrides.",
      .required = FALSE,
      .additional_properties = TRUE
    )

    expect_s3_class(schema, "ellmer::TypeObject")
  })

  test_that("incorrect required argument placement errors clearly", {
    skip_if_not_installed("ellmer")

    expect_error(
      ellmer::type_object("Spec overrides.", required = FALSE),
      "must be a list of <Type>s"
    )
  })
})
