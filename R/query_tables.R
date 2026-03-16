# Query Tables Helpers ----

normalize_character_array <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- trimws(values)
  values <- values[nzchar(values)]

  if (length(values) == 0) {
    stop(
      paste0("'", name, "' must include at least one non-empty value."),
      call. = FALSE
    )
  }

  values
}

contains_from_clause <- function(sql) {
  grepl("\\bFROM\\b", sql, ignore.case = TRUE, perl = TRUE)
}

contains_destructive_sql <- function(sql) {
  grepl(
    "(^|[^A-Z_])(INSERT|UPDATE|DELETE|DROP|ALTER|TRUNCATE|CREATE|REPLACE|ATTACH|DETACH|COPY|VACUUM|MERGE)\\b",
    toupper(sql),
    perl = TRUE
  )
}

normalize_table_candidate <- function(x) {
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    return(NA_character_)
  }

  value <- trimws(x)

  # Ignore derived table expressions/subqueries.
  if (startsWith(value, "(")) {
    return(NA_character_)
  }

  # Keep only first token (before alias/whitespace/comma).
  value <- strsplit(value, "[[:space:],]", perl = TRUE)[[1]][[1]]
  value <- gsub(";+$", "", value)
  value <- gsub("[)]+$", "", value)

  # Remove identifier quoting.
  value <- gsub('`', "", value)
  value <- gsub('"', "", value)
  value <- gsub("^\\[|\\]$", "", value)

  # If schema-qualified, use final component for dbExistsTable fallback.
  if (grepl("\\.", value, perl = TRUE)) {
    value <- strsplit(value, "\\.", perl = TRUE)[[1]]
    value <- value[[length(value)]]
  }

  value <- trimws(value)
  if (!nzchar(value)) {
    return(NA_character_)
  }

  value
}

extract_sql_table_candidates <- function(sql) {
  hits <- stringr::str_match_all(
    sql,
    stringr::regex(
      "\\b(?:FROM|JOIN)\\s+([^\\s,;]+(?:\\s*\\.[^\\s,;]+)?)",
      ignore_case = TRUE
    )
  )[[1]]

  if (nrow(hits) == 0) {
    return(character())
  }

  raw_candidates <- hits[, 2, drop = TRUE]
  normalized <- vapply(raw_candidates, normalize_table_candidate, character(1))
  unique(stats::na.omit(normalized))
}

extract_sql_cte_names <- function(sql) {
  # Capture CTE aliases from WITH / , alias AS (
  hits <- stringr::str_match_all(
    sql,
    stringr::regex(
      "(?:\\bWITH\\b(?:\\s+RECURSIVE)?|,)\\s+([A-Za-z_][A-Za-z0-9_]*|`[^`]+`|\"[^\"]+\")\\s+AS\\s*\\(",
      ignore_case = TRUE
    )
  )[[1]]

  if (nrow(hits) == 0) {
    return(character())
  }

  raw_names <- hits[, 2, drop = TRUE]
  normalized <- vapply(raw_names, normalize_table_candidate, character(1))
  unique(stats::na.omit(normalized))
}

get_recent_table_suggestions <- function(con, n = 3L) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) {
    n <- 3L
  }

  if (DBI::dbExistsTable(con, "table_metadata")) {
    recent <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT table_name FROM table_metadata ",
          "WHERE is_active = 1 ",
          "ORDER BY updated_at DESC ",
          "LIMIT ",
          n
        )
      ),
      error = function(e) NULL
    )

    if (
      !is.null(recent) && nrow(recent) > 0 && "table_name" %in% names(recent)
    ) {
      out <- as.character(recent$table_name)
      out <- out[nzchar(out)]
      if (length(out) > 0) {
        return(unique(out))
      }
    }
  }

  all_tables <- DBI::dbListTables(con)
  if (length(all_tables) == 0) {
    return(character())
  }

  head(rev(all_tables), n)
}

assert_free_sql_tables_exist <- function(con, sql) {
  candidates <- extract_sql_table_candidates(sql)
  cte_names <- extract_sql_cte_names(sql)

  if (length(cte_names) > 0) {
    candidates <- setdiff(candidates, cte_names)
  }

  if (length(candidates) == 0) {
    return(invisible(TRUE))
  }

  missing <- candidates[
    !vapply(
      candidates,
      function(tbl) {
        DBI::dbExistsTable(con, tbl)
      },
      logical(1)
    )
  ]

  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  recent <- get_recent_table_suggestions(con, n = 3)
  recent_txt <- if (length(recent) > 0) {
    paste0(
      " Recent tables: ",
      paste(recent, collapse = ", "),
      "."
    )
  } else {
    ""
  }

  stop(
    paste0(
      "Referenced table(s) not found: ",
      paste(unique(missing), collapse = ", "),
      ".",
      recent_txt,
      " Call get_data_table_metadata before query_tables to confirm names."
    ),
    call. = FALSE
  )
}

raise_query_tables_sql_error <- function(error, sql) {
  message <- conditionMessage(error)
  sql_upper <- toupper(sql)

  hints <- character()

  if (
    grepl("Could not ORDER BY column", message, fixed = TRUE) &&
      grepl("\\bUNION\\b", sql_upper, perl = TRUE)
  ) {
    hints <- c(
      hints,
      paste(
        "Tip: For UNION queries, ORDER BY expressions must be projected",
        "in each SELECT, or wrap UNION in a subquery/CTE and ORDER BY outside."
      )
    )
  }

  if (length(hints) == 0) {
    stop(message, call. = FALSE)
  }

  stop(
    paste(c(message, hints), collapse = " "),
    call. = FALSE
  )
}

query_tables_db_get_query <- function(con, sql) {
  tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(e) {
      raise_query_tables_sql_error(e, sql)
    }
  )
}

query_tables_db_execute <- function(con, sql) {
  tryCatch(
    DBI::dbExecute(con, sql),
    error = function(e) {
      raise_query_tables_sql_error(e, sql)
    }
  )
}

normalize_sql_mode <- function(mode, input_tables, sql) {
  if (is.null(mode) || !length(mode)) {
    if (!is.null(input_tables) && length(input_tables) > 0) {
      return("vectorized")
    }
    return("free")
  }

  mode <- tolower(trimws(as.character(mode[[1]])))
  if (!mode %in% c("vectorized", "free")) {
    stop("'mode' must be either 'vectorized' or 'free'.", call. = FALSE)
  }

  mode
}

normalize_result_rows <- function(df, max_rows = 200L) {
  if (nrow(df) == 0) {
    return(list(
      result_rows = list(),
      rows_returned = 0L,
      rows_total = 0L,
      result_truncated = FALSE
    ))
  }

  max_rows <- as.integer(max_rows)
  rows_total <- nrow(df)
  rows_returned <- min(rows_total, max_rows)
  truncated <- rows_total > max_rows

  rows <- df |>
    head(rows_returned) |>
    as.list() |>
    lapply(function(col) {
      if (inherits(col, "Date") || inherits(col, "POSIXt")) {
        as.character(col)
      } else {
        col
      }
    }) |>
    purrr::transpose()

  list(
    result_rows = rows,
    rows_returned = as.integer(rows_returned),
    rows_total = as.integer(rows_total),
    result_truncated = truncated
  )
}

run_query_tables <- function(
  con,
  mode = NULL,
  sql,
  input_tables = NULL,
  output_table_names = NULL,
  output_table_labels = NULL,
  persist = FALSE,
  add_data_view = TRUE,
  max_rows = 200L
) {
  if (is.null(sql) || length(sql) == 0) {
    sql <- ""
  }

  sql <- as.character(sql[[1]])
  sql <- trimws(sql)

  if (!nzchar(sql)) {
    stop("'sql' must be a non-empty SQL string.", call. = FALSE)
  }

  if (contains_destructive_sql(sql)) {
    stop(
      paste(
        "Destructive SQL statements are not allowed.",
        "Use SELECT-style read queries only."
      ),
      call. = FALSE
    )
  }

  input_tables <- normalize_character_array(input_tables, "input_tables")
  output_table_names <- normalize_character_array(
    output_table_names,
    "output_table_names"
  )
  output_table_labels <- normalize_character_array(
    output_table_labels,
    "output_table_labels"
  )

  mode <- normalize_sql_mode(mode, input_tables, sql)
  persist <- isTRUE(persist)
  add_data_view <- isTRUE(add_data_view)

  if (identical(mode, "free") && !is.null(input_tables)) {
    stop(
      "'free' mode does not allow 'input_tables'. Remove them or switch to 'vectorized'.",
      call. = FALSE
    )
  }

  if (identical(mode, "vectorized") && contains_from_clause(sql)) {
    stop(
      "'vectorized' mode SQL cannot include FROM. Use 'free' mode for SQL with FROM.",
      call. = FALSE
    )
  }

  if (identical(mode, "vectorized")) {
    if (is.null(input_tables)) {
      stop(
        "'vectorized' mode requires 'input_tables'.",
        call. = FALSE
      )
    }

    missing_tables <- input_tables[
      !vapply(
        input_tables,
        function(tbl) DBI::dbExistsTable(con, tbl),
        logical(1)
      )
    ]

    if (length(missing_tables) > 0) {
      stop(
        paste0(
          "Input table(s) not found: ",
          paste(unique(missing_tables), collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    if (is.null(output_table_names) || is.null(output_table_labels)) {
      stop(
        paste(
          "'vectorized' mode requires both 'output_table_names'",
          "and 'output_table_labels'."
        ),
        call. = FALSE
      )
    }

    n_inputs <- length(input_tables)
    n_names <- length(output_table_names)
    n_labels <- length(output_table_labels)

    if (!identical(n_inputs, n_names) || !identical(n_inputs, n_labels)) {
      stop(
        paste0(
          "Input lengths must match before querying: ",
          "length(input_tables)=",
          n_inputs,
          ", length(output_table_names)=",
          n_names,
          ", length(output_table_labels)=",
          n_labels,
          "."
        ),
        call. = FALSE
      )
    }

    queries <- purrr::map2(
      input_tables,
      output_table_names,
      function(input_table, output_table_name) {
        quoted_input <- as.character(DBI::dbQuoteIdentifier(con, input_table))
        statement <- paste0("SELECT * FROM ", quoted_input, " ", sql)

        list(
          input_table = input_table,
          output_table_name = output_table_name,
          statement = statement
        )
      }
    )

    if (persist) {
      purrr::walk2(
        queries,
        output_table_labels,
        function(query_item, output_table_label) {
          quoted_output <- as.character(
            DBI::dbQuoteIdentifier(con, query_item$output_table_name)
          )

          query_tables_db_execute(
            con,
            paste0(
              "CREATE OR REPLACE TABLE ",
              quoted_output,
              " AS ",
              query_item$statement
            )
          )

          row_count <- DBI::dbGetQuery(
            con,
            paste0("SELECT COUNT(*) AS n FROM ", quoted_output)
          )$n[[1]]

          column_count <- length(DBI::dbListFields(
            con,
            query_item$output_table_name
          ))

          upsert_table_metadata(
            con = con,
            table_name = query_item$output_table_name,
            table_label = output_table_label,
            source = "query_tables",
            source_detail = paste0(
              "tool=query_tables; mode=vectorized; input_table=",
              query_item$input_table,
              "; sql=",
              query_item$statement
            ),
            row_count = row_count,
            column_count = column_count,
            is_active = TRUE
          )
        }
      )

      variable_names <- purrr::map(
        output_table_names,
        function(tbl) DBI::dbListFields(con, tbl)
      )

      dimensions <- purrr::map(
        output_table_names,
        function(tbl) {
          list(
            nrow = as.integer(DBI::dbGetQuery(
              con,
              paste0(
                "SELECT COUNT(*) AS n FROM ",
                as.character(DBI::dbQuoteIdentifier(con, tbl))
              )
            )$n[[1]]),
            ncol = as.integer(length(DBI::dbListFields(con, tbl)))
          )
        }
      )

      return(list(
        mode = mode,
        table_name = output_table_names,
        table_label = output_table_labels,
        add_data_view = add_data_view,
        variable_names = variable_names,
        dimensions = dimensions
      ))
    }

    results <- purrr::map(queries, function(query_item) {
      query_tables_db_get_query(con, query_item$statement)
    })

    variable_names <- purrr::map(results, names)
    dimensions <- purrr::map(results, function(df) {
      list(
        nrow = as.integer(nrow(df)),
        ncol = as.integer(ncol(df))
      )
    })

    previews <- purrr::map(results, normalize_result_rows, max_rows = max_rows)

    list(
      mode = mode,
      input_tables = input_tables,
      table_name = output_table_names,
      table_label = output_table_labels,
      add_data_view = FALSE,
      variable_names = variable_names,
      dimensions = dimensions,
      result_rows = purrr::map(previews, "result_rows"),
      rows_returned = as.integer(unlist(purrr::map(previews, "rows_returned"))),
      rows_total = as.integer(unlist(purrr::map(previews, "rows_total"))),
      result_truncated = as.logical(unlist(purrr::map(
        previews,
        "result_truncated"
      )))
    )
  } else {
    assert_free_sql_tables_exist(con, sql)

    if (is.null(output_table_names) != is.null(output_table_labels)) {
      stop(
        "Provide both 'output_table_names' and 'output_table_labels', or neither.",
        call. = FALSE
      )
    }

    if (
      !is.null(output_table_names) &&
        length(output_table_names) != length(output_table_labels)
    ) {
      stop(
        "'output_table_names' and 'output_table_labels' must have matching lengths.",
        call. = FALSE
      )
    }

    if (!is.null(output_table_names) && length(output_table_names) > 1) {
      stop(
        "'free' mode supports at most one output table name/label.",
        call. = FALSE
      )
    }

    if (persist && is.null(output_table_names)) {
      stop(
        "'free' mode with persist=TRUE requires one output table name and label.",
        call. = FALSE
      )
    }

    if (persist) {
      output_table_name <- output_table_names[[1]]
      output_table_label <- output_table_labels[[1]]
      quoted_output <- as.character(DBI::dbQuoteIdentifier(
        con,
        output_table_name
      ))

      query_tables_db_execute(
        con,
        paste0("CREATE OR REPLACE TABLE ", quoted_output, " AS ", sql)
      )

      row_count <- DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*) AS n FROM ", quoted_output)
      )$n[[1]]

      column_count <- length(DBI::dbListFields(con, output_table_name))

      upsert_table_metadata(
        con = con,
        table_name = output_table_name,
        table_label = output_table_label,
        source = "query_tables",
        source_detail = paste0(
          "tool=query_tables; mode=free; sql=",
          sql
        ),
        row_count = row_count,
        column_count = column_count,
        is_active = TRUE
      )

      return(list(
        mode = mode,
        table_name = output_table_name,
        table_label = output_table_label,
        add_data_view = add_data_view,
        variable_names = DBI::dbListFields(con, output_table_name),
        dimensions = list(
          nrow = as.integer(row_count),
          ncol = as.integer(column_count)
        )
      ))
    }

    result <- query_tables_db_get_query(con, sql)

    preview <- normalize_result_rows(result, max_rows = max_rows)

    list(
      mode = mode,
      add_data_view = FALSE,
      variable_names = names(result),
      dimensions = list(
        nrow = as.integer(nrow(result)),
        ncol = as.integer(ncol(result))
      ),
      result_rows = preview$result_rows,
      rows_returned = preview$rows_returned,
      rows_total = preview$rows_total,
      result_truncated = preview$result_truncated
    )
  }
}
