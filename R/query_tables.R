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

normalize_query_result_presentation <- function(result_presentation) {
  if (is.null(result_presentation) || !length(result_presentation)) {
    return("default")
  }

  value <- tolower(trimws(as.character(result_presentation[[1]])))
  if (!value %in% c("default", "table")) {
    stop(
      "'result_presentation' must be either 'default' or 'table'.",
      call. = FALSE
    )
  }

  value
}

format_query_table_cell <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return("")
  }

  if (length(x) == 1 && is.na(x)) {
    return("")
  }

  if (is.list(x)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }

  as.character(x[[1]])
}

query_preview_rows_to_df <- function(result_rows, variable_names) {
  variable_names <- as.character(variable_names)

  if (length(variable_names) == 0) {
    return(data.frame())
  }

  if (is.null(result_rows) || length(result_rows) == 0) {
    empty_cols <- stats::setNames(
      vector("list", length(variable_names)),
      variable_names
    )
    return(as.data.frame(empty_cols, stringsAsFactors = FALSE))
  }

  row_records <- lapply(result_rows, function(row) {
    out <- stats::setNames(
      vector("list", length(variable_names)),
      variable_names
    )
    row_list <- as.list(row)

    row_names <- names(row_list)
    if (!is.null(row_names) && any(nzchar(row_names))) {
      matched_names <- intersect(variable_names, row_names)
      for (col_name in matched_names) {
        out[[col_name]] <- row_list[[col_name]]
      }

      return(out)
    }

    values <- unname(row_list)

    n_values <- min(length(values), length(variable_names))
    if (n_values > 0) {
      out[seq_len(n_values)] <- values[seq_len(n_values)]
    }

    out
  })

  row_dfs <- lapply(
    row_records,
    function(record) {
      as.data.frame(record, stringsAsFactors = FALSE, check.names = FALSE)
    }
  )

  rows_df <- if (length(row_dfs) == 1) {
    row_dfs[[1]]
  } else {
    Reduce(
      function(lhs, rhs) {
        rbind(lhs, rhs)
      },
      row_dfs
    )
  }

  missing_cols <- setdiff(variable_names, names(rows_df))
  if (length(missing_cols) > 0) {
    for (col_name in missing_cols) {
      rows_df[[col_name]] <- NA_character_
    }
  }

  rows_df[, variable_names, drop = FALSE]
}

build_query_table_html <- function(df, title = NULL, footer = NULL) {
  header <- shiny::tags$thead(
    shiny::tags$tr(
      lapply(names(df), function(column_name) {
        shiny::tags$th(column_name)
      })
    )
  )

  body_rows <- if (nrow(df) == 0) {
    list(
      shiny::tags$tr(
        shiny::tags$td(
          colspan = max(length(names(df)), 1),
          "No rows returned."
        )
      )
    )
  } else {
    lapply(seq_len(nrow(df)), function(i) {
      shiny::tags$tr(
        lapply(df[i, , drop = FALSE], function(cell) {
          shiny::tags$td(format_query_table_cell(cell))
        })
      )
    })
  }

  table_tag <- shiny::tags$table(
    class = "table table-sm table-striped table-bordered",
    header,
    shiny::tags$tbody(body_rows)
  )

  shiny::tags$div(
    if (!is.null(title)) shiny::tags$h5(title),
    table_tag,
    if (!is.null(footer)) shiny::tags$small(class = "text-muted", footer)
  )
}

build_query_tables_table_display <- function(result) {
  if (is.null(result$result_rows) || is.null(result$variable_names)) {
    return(NULL)
  }

  is_vectorized_preview <- is.list(result$variable_names) &&
    !is.null(result$mode) &&
    identical(result$mode, "vectorized")

  if (!is_vectorized_preview) {
    rows_df <- query_preview_rows_to_df(
      result$result_rows,
      result$variable_names
    )
    footer <- paste0(
      "Preview rows: ",
      result$rows_returned,
      " of ",
      result$rows_total,
      if (isTRUE(result$result_truncated)) " (truncated)." else "."
    )

    return(build_query_table_html(
      rows_df,
      title = "Query Results",
      footer = footer
    ))
  }

  table_names <- result$table_label
  if (is.null(table_names) || length(table_names) == 0) {
    table_names <- result$table_name
  }

  table_blocks <- lapply(seq_along(result$result_rows), function(i) {
    title <- if (!is.null(table_names) && length(table_names) >= i) {
      as.character(table_names[[i]])
    } else {
      paste0("Result ", i)
    }

    rows_returned <- if (
      !is.null(result$rows_returned) && length(result$rows_returned) >= i
    ) {
      result$rows_returned[[i]]
    } else {
      0L
    }

    rows_total <- if (
      !is.null(result$rows_total) && length(result$rows_total) >= i
    ) {
      result$rows_total[[i]]
    } else {
      0L
    }

    truncated <- if (
      !is.null(result$result_truncated) && length(result$result_truncated) >= i
    ) {
      isTRUE(result$result_truncated[[i]])
    } else {
      FALSE
    }

    footer <- paste0(
      "Preview rows: ",
      rows_returned,
      " of ",
      rows_total,
      if (truncated) " (truncated)." else "."
    )

    rows_df <- query_preview_rows_to_df(
      result_rows = result$result_rows[[i]],
      variable_names = result$variable_names[[i]]
    )

    build_query_table_html(rows_df, title = title, footer = footer)
  })

  do.call(shiny::tagList, table_blocks)
}

get_query_tables_result_presenters <- function() {
  list(
    default = function(result) NULL,
    table = function(result) {
      html <- build_query_tables_table_display(result)
      if (is.null(html)) {
        return(NULL)
      }

      list(
        html = html,
        show_request = FALSE,
        open = TRUE,
        title = "Query Result Preview"
      )
    }
  )
}

build_query_tables_display <- function(result, result_presentation) {
  presenters <- get_query_tables_result_presenters()
  presenter <- presenters[[result_presentation]]

  if (is.null(presenter)) {
    stop(
      paste0(
        "No presenter registered for result_presentation='",
        result_presentation,
        "'."
      ),
      call. = FALSE
    )
  }

  presenter(result)
}

finalize_query_tables_result <- function(result, result_presentation) {
  display <- build_query_tables_display(
    result = result,
    result_presentation = result_presentation
  )

  if (is.null(display)) {
    return(result)
  }

  content_tool_result <- get("ContentToolResult", envir = asNamespace("ellmer"))

  content_tool_result(
    value = result,
    extra = list(display = display)
  )
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
  max_rows = 200L,
  result_presentation = "default"
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
  result_presentation <- normalize_query_result_presentation(
    result_presentation
  )
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

      return(finalize_query_tables_result(
        list(
          mode = mode,
          table_name = output_table_names,
          table_label = output_table_labels,
          add_data_view = add_data_view,
          variable_names = variable_names,
          dimensions = dimensions
        ),
        result_presentation
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

    finalize_query_tables_result(
      list(
        mode = mode,
        input_tables = input_tables,
        table_name = output_table_names,
        table_label = output_table_labels,
        add_data_view = FALSE,
        variable_names = variable_names,
        dimensions = dimensions,
        result_rows = purrr::map(previews, "result_rows"),
        rows_returned = as.integer(unlist(purrr::map(
          previews,
          "rows_returned"
        ))),
        rows_total = as.integer(unlist(purrr::map(previews, "rows_total"))),
        result_truncated = as.logical(unlist(purrr::map(
          previews,
          "result_truncated"
        )))
      ),
      result_presentation
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

      return(finalize_query_tables_result(
        list(
          mode = mode,
          table_name = output_table_name,
          table_label = output_table_label,
          add_data_view = add_data_view,
          variable_names = DBI::dbListFields(con, output_table_name),
          dimensions = list(
            nrow = as.integer(row_count),
            ncol = as.integer(column_count)
          )
        ),
        result_presentation
      ))
    }

    result <- query_tables_db_get_query(con, sql)

    preview <- normalize_result_rows(result, max_rows = max_rows)

    finalize_query_tables_result(
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
      ),
      result_presentation
    )
  }
}
