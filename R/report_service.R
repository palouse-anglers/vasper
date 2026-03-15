# Report Service Helpers ----

#' Format report year labels
#'
#' @param year_range Integer/numeric vector of length 2
#'
#' @return Character scalar year label
#' @export
format_year_label <- function(year_range) {
  if (length(year_range) != 2 || any(is.na(year_range))) {
    return(NA_character_)
  }

  start_year <- as.integer(year_range[1])
  end_year <- as.integer(year_range[2])

  if (start_year == end_year) {
    as.character(start_year)
  } else {
    glue::glue("{start_year}-{end_year}")
  }
}

#' Build report download filename
#'
#' @param report_format Character scalar in c("html", "docx", "all")
#' @param producer_id Character scalar producer id
#' @param report_depth Character scalar depth like "0-3"
#' @param report_year_range Integer/numeric vector length 2
#'
#' @return Character scalar filename
#' @export
build_report_filename <- function(
  report_format,
  producer_id,
  report_depth,
  report_year_range
) {
  ext <- if (identical(report_format, "all")) "zip" else report_format
  depth_suffix <- gsub("-", "to", report_depth)
  year_label <- format_year_label(report_year_range)

  glue::glue("{year_label}_{producer_id}_{depth_suffix}_Report.{ext}")
}

#' Get ordered producer choices for reports
#'
#' @param con DBI connection
#' @param data_table Character scalar table name
#'
#' @return Character vector producer ids
#' @export
get_report_producers <- function(con, data_table = TABLE_NAMES$soil_data) {
  query <- glue::glue(
    "SELECT DISTINCT producer_id FROM {data_table} ORDER BY producer_id"
  )

  DBI::dbGetQuery(con, query)$producer_id
}

#' Get min/max available years for a producer
#'
#' @param con DBI connection
#' @param producer_id Character scalar
#' @param data_table Character scalar table name
#'
#' @return Integer vector length 2 c(min_year, max_year) or integer(0)
#' @export
get_report_year_bounds <- function(
  con,
  producer_id,
  data_table = TABLE_NAMES$soil_data
) {
  query <- glue::glue(
    "SELECT DISTINCT year FROM {data_table} WHERE producer_id = ? ORDER BY year"
  )

  years <- DBI::dbGetQuery(con, query, params = list(producer_id))$year

  if (length(years) == 0) {
    return(integer())
  }

  c(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
}

#' Get available depths for producer/year range
#'
#' @param con DBI connection
#' @param producer_id Character scalar
#' @param year_start Integer scalar
#' @param year_end Integer scalar
#' @param data_table Character scalar table name
#'
#' @return Character vector of depth values
#' @export
get_report_depths <- function(
  con,
  producer_id,
  year_start,
  year_end,
  data_table = TABLE_NAMES$soil_data
) {
  query <- glue::glue(
    "SELECT DISTINCT depth FROM {data_table}
       WHERE producer_id = ?
         AND year BETWEEN ? AND ?
       ORDER BY depth"
  )

  DBI::dbGetQuery(
    con,
    query,
    params = list(producer_id, as.integer(year_start), as.integer(year_end))
  )$depth
}
