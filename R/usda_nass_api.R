# USDA NASS QuickStats API integration

#' Get USDA NASS API credentials from environment
#'
#' @return Named list containing API key
#' @export
get_usda_nass_credentials <- function() {
  api_key <- Sys.getenv("USDA_NASS_API_KEY")

  if (!nzchar(api_key)) {
    stop(
      "Missing USDA NASS API key. Set USDA_NASS_API_KEY in .Renviron.",
      call. = FALSE
    )
  }

  list(api_key = api_key)
}

#' Configure USDA NASS request rate limiting
#'
#' @param rate_seconds Minimum seconds between requests
#' @param throttled_rate_seconds Seconds between requests when throttled
#' @param override Whether to bypass waiting
#' @param reset_throttling Whether to reset throttled mode
#'
#' @return Invisibly returns previous settings
#' @export
set_usda_nass_rate_limit <- function(
  rate_seconds = 0.2,
  throttled_rate_seconds = 3,
  override = FALSE,
  reset_throttling = FALSE
) {
  old <- list(
    rate_seconds = getOption("vasper.nass.rate_seconds", 0.2),
    throttled_rate_seconds = getOption("vasper.nass.throttled_rate_seconds", 3),
    override = getOption("vasper.nass.override_rate_limit", FALSE),
    is_throttled = getOption("vasper.nass.is_throttled", FALSE)
  )

  options(
    vasper.nass.rate_seconds = rate_seconds,
    vasper.nass.throttled_rate_seconds = throttled_rate_seconds,
    vasper.nass.override_rate_limit = override
  )

  if (isTRUE(reset_throttling)) {
    options(vasper.nass.is_throttled = FALSE)
  }

  invisible(old)
}

#' Apply USDA NASS request pacing
#'
#' @return Invisibly returns wait time in seconds
#' @export
usda_nass_rate_limit <- function() {
  if (isTRUE(getOption("vasper.nass.override_rate_limit", FALSE))) {
    return(invisible(0))
  }

  is_throttled <- isTRUE(getOption("vasper.nass.is_throttled", FALSE))
  current_rate <- if (is_throttled) {
    getOption("vasper.nass.throttled_rate_seconds", 3)
  } else {
    getOption("vasper.nass.rate_seconds", 0.2)
  }

  last_request_time <- getOption("vasper.nass.last_request_time", NULL)
  if (is.null(last_request_time)) {
    options(vasper.nass.last_request_time = Sys.time())
    return(invisible(0))
  }

  elapsed <- as.numeric(difftime(Sys.time(), last_request_time, units = "secs"))
  wait_time <- current_rate - elapsed
  if (wait_time > 0) {
    Sys.sleep(wait_time)
  } else {
    wait_time <- 0
  }

  options(vasper.nass.last_request_time = Sys.time())
  invisible(wait_time)
}

#' Validate USDA NASS query parameter names
#'
#' @param query Named list of query parameters
#' @param endpoint Endpoint name
#'
#' @return Invisibly TRUE
#' @export
validate_usda_nass_query <- function(
  query,
  endpoint = c("api_GET", "get_counts", "get_param_values")
) {
  endpoint <- match.arg(endpoint)

  allowed <- c(
    "agg_level_desc",
    "asd_code",
    "asd_desc",
    "begin_code",
    "class_desc",
    "commodity_desc",
    "congr_district_code",
    "country_code",
    "country_name",
    "county_ansi",
    "county_code",
    "county_name",
    "domaincat_desc",
    "domain_desc",
    "end_code",
    "freq_desc",
    "group_desc",
    "load_time",
    "location_desc",
    "prodn_practice_desc",
    "reference_period_desc",
    "region_desc",
    "sector_desc",
    "short_desc",
    "source_desc",
    "state_alpha",
    "state_ansi",
    "state_fips_code",
    "state_name",
    "statisticcat_desc",
    "unit_desc",
    "util_practice_desc",
    "watershed_code",
    "watershed_desc",
    "week_ending",
    "year",
    "zip_5",
    "param"
  )

  if (is.null(names(query)) || any(!nzchar(names(query)))) {
    stop("USDA NASS query must be a named list.", call. = FALSE)
  }

  stripped <- names(query) |>
    toupper()
  stripped <- gsub("__LE|__LT|__GT|__GE|__LIKE|__NOT_LIKE|__NE", "", stripped)

  allowed_upper <- toupper(allowed)
  if (endpoint != "get_param_values") {
    allowed_upper <- setdiff(allowed_upper, "PARAM")
  }

  bad <- unique(names(query)[!(stripped %in% allowed_upper)])

  if (length(bad) > 0) {
    stop(
      paste0(
        "Invalid USDA NASS parameter(s): ",
        paste(bad, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

normalize_usda_nass_query_values <- function(query) {
  lapply(query, function(value) {
    if (is.null(value)) {
      return(NULL)
    }

    flattened <- if (is.list(value)) {
      unlist(value, recursive = TRUE, use.names = FALSE)
    } else {
      value
    }

    if (!is.atomic(flattened)) {
      stop(
        "USDA NASS query values must be atomic vectors or scalar values.",
        call. = FALSE
      )
    }

    if (length(flattened) == 0) {
      return(NULL)
    }

    if (is.factor(flattened)) {
      flattened <- as.character(flattened)
    }

    flattened
  })
}

#' Stop with clearer errors for USDA NASS HTTP responses
#'
#' @param response httr2 response
#'
#' @return Invisibly TRUE when no error
#' @export
check_usda_nass_response <- function(response) {
  status <- httr2::resp_status(response)

  if (status < 400) {
    return(invisible(TRUE))
  }

  body_txt <- tryCatch(
    httr2::resp_body_string(response),
    error = function(e) ""
  )

  if (status == 413) {
    stop(
      paste(
        "Request was too large.",
        "NASS requires that an API call returns a maximum of 50,000 records.",
        "Subset by geography or year."
      ),
      call. = FALSE
    )
  }

  if (status == 429) {
    stop(
      "Requests sent too quickly. Error (429) persists despite retries.",
      call. = FALSE
    )
  }

  stop(
    paste0(
      "USDA NASS HTTP error ",
      status,
      if (nzchar(body_txt)) paste0(": ", body_txt) else ""
    ),
    call. = FALSE
  )
}

#' Build and execute USDA NASS QuickStats API request
#'
#' @param query Named list of API query parameters
#' @param endpoint API endpoint name: 'api_GET' or 'get_counts'
#' @param timeout_seconds Request timeout in seconds
#'
#' @return Parsed JSON payload
#' @export
request_usda_nass_api <- function(
  query = list(),
  endpoint = c("api_GET", "get_counts", "get_param_values"),
  timeout_seconds = 30,
  format = c("JSON", "CSV"),
  fallback_csv = TRUE,
  max_retries = 2
) {
  endpoint <- match.arg(endpoint)
  format <- match.arg(format)

  validate_usda_nass_query(query = query, endpoint = endpoint)
  query <- normalize_usda_nass_query_values(query)
  creds <- get_usda_nass_credentials()

  formats_to_try <- if (identical(format, "JSON") && isTRUE(fallback_csv)) {
    c("JSON", "CSV")
  } else {
    format
  }

  last_error <- NULL

  for (fmt in formats_to_try) {
    req_query <- c(
      list(
        key = creds$api_key,
        format = fmt
      ),
      query
    )

    for (attempt in seq_len(max_retries + 1)) {
      usda_nass_rate_limit()

      req <- httr2::request(
        paste0("https://quickstats.nass.usda.gov/api/", endpoint, "/")
      ) |>
        httr2::req_url_query(!!!req_query, .multi = "explode") |>
        httr2::req_user_agent(
          "vasper/0.1 (+https://github.com/palouse-anglers/vasper)"
        ) |>
        httr2::req_headers(Connection = "close") |>
        httr2::req_timeout(timeout_seconds)

      resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) e
      )

      if (inherits(resp, "error")) {
        last_error <- resp
        break
      }

      status <- httr2::resp_status(resp)

      if (status == 429 && attempt <= max_retries) {
        options(vasper.nass.is_throttled = TRUE)
        Sys.sleep(getOption("vasper.nass.throttled_rate_seconds", 3))
        next
      }

      check_usda_nass_response(resp)
      options(vasper.nass.is_throttled = FALSE)

      parsed <- if (fmt == "JSON") {
        tryCatch(
          httr2::resp_body_json(resp, simplifyVector = TRUE),
          error = function(e) e
        )
      } else {
        txt <- httr2::resp_body_string(resp)
        utils::read.csv(
          text = txt,
          stringsAsFactors = FALSE,
          colClasses = "character"
        )
      }

      if (inherits(parsed, "error")) {
        last_error <- parsed
        break
      }

      return(parsed)
    }
  }

  stop(
    if (!is.null(last_error)) {
      conditionMessage(last_error)
    } else {
      "USDA NASS request failed."
    },
    call. = FALSE
  )
}

#' Get USDA NASS record count for a query
#'
#' @param query Named list of API query parameters
#' @param timeout_seconds Request timeout in seconds
#'
#' @return Integer record count
#' @export
get_usda_nass_count <- function(query = list(), timeout_seconds = 30) {
  payload <- request_usda_nass_api(
    query = query,
    endpoint = "get_counts",
    timeout_seconds = timeout_seconds
  )

  if (is.data.frame(payload)) {
    count_col <- names(payload)[tolower(names(payload)) == "count"]
    if (length(count_col) == 0) {
      stop(
        "USDA NASS count response did not include a 'count' field.",
        call. = FALSE
      )
    }
    return(as.integer(payload[[count_col[[1]]]][[1]]))
  }

  if (is.list(payload) && "count" %in% names(payload)) {
    return(as.integer(payload$count[[1]]))
  }

  stop(
    "USDA NASS count response did not include a 'count' field.",
    call. = FALSE
  )
}

#' Parse NASS value string into numeric and suppression metadata
#'
#' @param value_raw Character vector of raw NASS values
#'
#' @return Tibble with value_raw, value_num, and is_suppressed
#' @export
parse_nass_value <- function(value_raw) {
  value_chr <- as.character(value_raw)
  value_trim <- stringr::str_trim(value_chr)
  value_upper <- toupper(value_trim)

  suppressed_codes <- c("", "(D)", "(H)", "(L)", "(NA)", "(X)")
  is_suppressed <- is.na(value_trim) | value_upper %in% suppressed_codes

  cleaned <- stringr::str_remove_all(value_trim, ",")
  cleaned[value_upper == "(Z)"] <- "0"
  cleaned[is_suppressed] <- NA_character_

  value_num <- suppressWarnings(as.numeric(cleaned))

  tibble::tibble(
    value_raw = value_chr,
    value_num = value_num,
    is_suppressed = is_suppressed
  )
}

#' Return fallback when value is NULL
#'
#' @param x Value to check
#' @param default Default value when x is NULL
#'
#' @return x when not NULL, otherwise default
#' @export
nass_value_or <- function(x, default) {
  if (is.null(x)) default else x
}

empty_nass_raw_tbl <- function() {
  tibble::tibble(
    year = integer(),
    commodity_desc = character(),
    class_desc = character(),
    util_practice_desc = character(),
    statisticcat_desc = character(),
    unit_desc = character(),
    short_desc = character(),
    value = character(),
    value_raw = character(),
    value_num = double(),
    is_suppressed = logical(),
    load_timestamp = as.POSIXct(character(), tz = "UTC")
  )
}

empty_nass_trend_tbl <- function() {
  tibble::tibble(
    year = integer(),
    commodity_desc = character(),
    class_desc = character(),
    util_practice_desc = character(),
    statisticcat_desc = character(),
    unit_desc = character(),
    rows_total = integer(),
    rows_numeric = integer(),
    rows_suppressed = integer(),
    value_mean = double(),
    value_min = double(),
    value_max = double(),
    value_sum = double()
  )
}

#' Fetch Columbia County, WA USDA NASS crop records
#'
#' @param crops Optional character vector of crop commodity names
#' @param statistics Character vector of statistic categories
#' @param year_min Minimum year
#' @param year_max Maximum year
#'
#' @return Tibble with normalized NASS raw records
#' @export
get_columbia_county_nass_raw <- function(
  crops = NULL,
  statistics = c("YIELD", "PRODUCTION", "AREA HARVESTED"),
  year_min = 1980,
  year_max = as.integer(format(Sys.Date(), "%Y"))
) {
  stopifnot(is.numeric(year_min), is.numeric(year_max), year_min <= year_max)

  statistics <- toupper(trimws(as.character(statistics)))
  statistics <- statistics[nzchar(statistics)]

  if (length(statistics) == 0) {
    stop(
      "'statistics' must include at least one statistic category.",
      call. = FALSE
    )
  }

  query <- list(
    source_desc = "SURVEY",
    sector_desc = "CROPS",
    state_alpha = "WA",
    state_ansi = "53",
    county_ansi = "013",
    county_name = "COLUMBIA",
    agg_level_desc = "COUNTY",
    year__GE = as.integer(year_min),
    year__LE = as.integer(year_max),
    statisticcat_desc = statistics
  )

  if (!is.null(crops) && length(crops) > 0) {
    crops <- toupper(trimws(as.character(crops)))
    crops <- crops[nzchar(crops)]

    if (length(crops) > 0) {
      query$commodity_desc <- crops
    }
  }

  row_count <- get_usda_nass_count(query)

  if (is.na(row_count) || row_count == 0) {
    return(empty_nass_raw_tbl())
  }

  if (row_count > 50000) {
    stop(
      paste0(
        "USDA NASS query would return ",
        row_count,
        " rows, exceeding API limit of 50,000. Narrow filters or date range."
      ),
      call. = FALSE
    )
  }

  payload <- request_usda_nass_api(query = query, endpoint = "api_GET")

  raw_data <- if (is.data.frame(payload)) {
    tibble::as_tibble(payload)
  } else if (is.list(payload) && "data" %in% names(payload)) {
    rows <- nass_value_or(payload$data, list())
    if (length(rows) == 0) {
      return(empty_nass_raw_tbl())
    }
    tibble::as_tibble(rows)
  } else {
    stop(
      "USDA NASS response did not include expected data content.",
      call. = FALSE
    )
  }

  cleaned_names <- names(raw_data) |>
    tolower()
  cleaned_names <- gsub("[^a-z0-9]+", "_", cleaned_names)
  cleaned_names <- gsub("(^_+|_+$)", "", cleaned_names)

  names(raw_data) <- cleaned_names

  if (!"value" %in% names(raw_data)) {
    raw_data$value <- NA_character_
  }

  parsed_values <- parse_nass_value(raw_data$value)

  raw_data <- raw_data |>
    dplyr::bind_cols(parsed_values) |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(year)),
      load_timestamp = as.POSIXct(Sys.time(), tz = "UTC")
    )

  raw_data |>
    dplyr::arrange(year, commodity_desc, statisticcat_desc, short_desc)
}

#' Summarize USDA NASS raw rows into trend rows
#'
#' @param raw_data Tibble from get_columbia_county_nass_raw
#'
#' @return Tibble of yearly trend aggregates
#' @export
get_columbia_county_nass_trends <- function(raw_data) {
  if (nrow(raw_data) == 0) {
    return(empty_nass_trend_tbl())
  }

  raw_data |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(year))
    ) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::summarize(
      rows_total = dplyr::n(),
      rows_numeric = sum(!is.na(value_num)),
      rows_suppressed = sum(is_suppressed, na.rm = TRUE),
      value_mean = ifelse(
        rows_numeric > 0,
        mean(value_num, na.rm = TRUE),
        NA_real_
      ),
      value_min = ifelse(
        rows_numeric > 0,
        min(value_num, na.rm = TRUE),
        NA_real_
      ),
      value_max = ifelse(
        rows_numeric > 0,
        max(value_num, na.rm = TRUE),
        NA_real_
      ),
      value_sum = ifelse(
        rows_numeric > 0,
        sum(value_num, na.rm = TRUE),
        NA_real_
      ),
      .by = c(
        year,
        commodity_desc,
        class_desc,
        util_practice_desc,
        statisticcat_desc,
        unit_desc
      )
    ) |>
    dplyr::arrange(
      year,
      commodity_desc,
      statisticcat_desc,
      class_desc,
      util_practice_desc
    )
}

#' Write USDA NASS yield raw and trend data to DuckDB
#'
#' @param raw_data Tibble of raw NASS rows
#' @param trend_data Tibble of trend NASS rows
#' @param con DBI connection
#' @param param_hash Parameter hash for table naming
#' @param table_label User-facing label root for Data page
#' @param add_data_view Whether to add tables to open Data views
#'
#' @return List describing both output tables
#' @export
write_yield_to_db <- function(
  raw_data,
  trend_data,
  con,
  param_hash,
  table_label,
  add_data_view = TRUE
) {
  if (!is.data.frame(raw_data) || ncol(raw_data) == 0) {
    raw_data <- empty_nass_raw_tbl()
  }

  if (!is.data.frame(trend_data) || ncol(trend_data) == 0) {
    trend_data <- empty_nass_trend_tbl()
  }

  raw_table_name <- paste0("usda_yields_raw_", param_hash)
  trend_table_name <- paste0("usda_yields_trend_", param_hash)

  if (!DBI::dbExistsTable(con, raw_table_name)) {
    DBI::dbWriteTable(con, raw_table_name, raw_data, overwrite = FALSE)
  }

  if (!DBI::dbExistsTable(con, trend_table_name)) {
    DBI::dbWriteTable(con, trend_table_name, trend_data, overwrite = FALSE)
  }

  upsert_table_metadata(
    con = con,
    table_name = raw_table_name,
    table_label = paste0(table_label, " (Raw)"),
    source = "usda_nass_raw",
    source_detail = paste0(
      "tool=get_yield_historical_nass; table_role=raw; param_hash=",
      param_hash
    ),
    row_count = nrow(raw_data),
    column_count = ncol(raw_data),
    is_active = TRUE
  )

  upsert_table_metadata(
    con = con,
    table_name = trend_table_name,
    table_label = paste0(table_label, " (Trend)"),
    source = "usda_nass_trend",
    source_detail = paste0(
      "tool=get_yield_historical_nass; table_role=trend; param_hash=",
      param_hash
    ),
    row_count = nrow(trend_data),
    column_count = ncol(trend_data),
    is_active = TRUE
  )

  list(
    table_name = c(raw_table_name, trend_table_name),
    table_label = c(
      paste0(table_label, " (Raw)"),
      paste0(table_label, " (Trend)")
    ),
    add_data_view = isTRUE(add_data_view),
    variable_names = list(
      names(raw_data),
      names(trend_data)
    ),
    dimensions = list(
      list(
        nrow = as.integer(nrow(raw_data)),
        ncol = as.integer(ncol(raw_data))
      ),
      list(
        nrow = as.integer(nrow(trend_data)),
        ncol = as.integer(ncol(trend_data))
      )
    )
  )
}
