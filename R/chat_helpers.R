# Chat Helpers ----

normalize_table_names <- function(x) {
  flat <- unlist(x, recursive = TRUE, use.names = FALSE)
  flat <- as.character(flat)
  flat <- trimws(flat)
  flat <- flat[nzchar(flat)]
  unique(flat)
}

sanitize_chat_text_scalar <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    return(NA_character_)
  }

  txt <- trimws(x)
  if (!nzchar(txt)) {
    return(NA_character_)
  }

  key <- tolower(gsub("\\s+", " ", txt))
  nullish_tokens <- c(
    "na",
    "n/a",
    "nan",
    "null",
    "none",
    "nil",
    "undefined",
    "user na",
    "assistant na",
    "(null)",
    "(none)",
    "(empty)"
  )

  if (key %in% nullish_tokens) {
    return(NA_character_)
  }

  txt
}

extract_turn_text_parts <- function(turn) {
  contents <- tryCatch(turn@contents, error = function(e) list())
  if (!is.list(contents) || length(contents) == 0) {
    return(character())
  }

  text_parts <- vapply(
    contents,
    function(content) {
      txt <- tryCatch(content@text, error = function(e) NA_character_)
      sanitize_chat_text_scalar(txt)
    },
    character(1)
  )

  text_parts[!is.na(text_parts)]
}

normalize_turn_role <- function(turn) {
  role <- tryCatch(turn@role, error = function(e) NA_character_)

  if (!is.character(role) || length(role) != 1) {
    return(NA_character_)
  }

  role <- tolower(trimws(role))
  if (!role %in% c("user", "assistant")) {
    return(NA_character_)
  }

  role
}

sanitize_turns_for_replay <- function(turns) {
  if (!is.list(turns) || length(turns) == 0) {
    return(list())
  }

  keep <- vapply(
    turns,
    function(turn) {
      role <- normalize_turn_role(turn)
      text_parts <- extract_turn_text_parts(turn)

      !is.na(role) && length(text_parts) > 0
    },
    logical(1)
  )

  turns[keep]
}

unwrap_tool_result <- function(x) {
  if (inherits(x, "ellmer::ContentToolResult")) {
    return(unwrap_tool_result(x@value))
  }

  if (
    is.list(x) &&
      !is.null(x$value) &&
      !is.null(x$request) &&
      !is.null(x$tool)
  ) {
    return(unwrap_tool_result(x$value))
  }

  x
}

deep_extract <- function(x, key) {
  out <- list()

  if (!is.list(x)) {
    return(out)
  }

  if (!is.null(names(x)) && key %in% names(x)) {
    out <- c(out, list(x[[key]]))
  }

  for (el in x) {
    out <- c(out, deep_extract(el, key))
  }

  out
}

parse_tool_result_data_view <- function(result) {
  result <- unwrap_tool_result(result)

  if (!is.list(result)) {
    return(list(
      should_queue = FALSE,
      table_names = character(),
      add_flags = logical()
    ))
  }

  # Only honor explicit top-level add_data_view/table_name fields from
  # write-to-DB tools. This avoids accidentally queueing views from nested
  # metadata payloads (e.g., table catalogs that include table_name keys).
  if (is.null(result$add_data_view)) {
    return(list(
      should_queue = FALSE,
      table_names = character(),
      add_flags = logical()
    ))
  }

  add_flags <- unlist(result$add_data_view, use.names = FALSE)

  table_names <- normalize_table_names(c(
    unlist(result$table_name %||% character(), use.names = FALSE),
    unlist(result$table_names %||% character(), use.names = FALSE)
  ))

  if (length(table_names) == 0) {
    return(list(
      should_queue = FALSE,
      table_names = character(),
      add_flags = add_flags
    ))
  }

  raw_flags <- add_flags
  add_flags <- suppressWarnings(as.logical(add_flags))

  explicit_false <- length(raw_flags) > 0 &&
    all(
      tolower(trimws(as.character(raw_flags))) %in%
        c("false", "f", "0", "no", "n")
    )

  should_queue <- if (explicit_false) {
    FALSE
  } else if (length(add_flags) == 0 || all(is.na(add_flags))) {
    TRUE
  } else {
    any(add_flags, na.rm = TRUE)
  }

  list(
    should_queue = should_queue,
    table_names = table_names,
    add_flags = raw_flags
  )
}

parse_tool_result_plot_artifact <- function(result) {
  result <- unwrap_tool_result(result)

  empty <- list(
    has_plot = FALSE,
    png_path = NA_character_,
    artifact_id = NA_character_,
    artifact_label = NA_character_
  )

  if (!is.list(result)) {
    return(empty)
  }

  files <- result$files %||% NULL
  png_path <- if (is.list(files)) {
    sanitize_chat_text_scalar(as.character(files$png %||% NA_character_)[[1]])
  } else {
    NA_character_
  }

  if (is.na(png_path)) {
    return(empty)
  }

  artifact_type <- tolower(trimws(as.character(
    result$artifact_type %||% ""
  )[[1]]))

  if (nzchar(artifact_type) && !identical(artifact_type, "plot")) {
    return(empty)
  }

  list(
    has_plot = TRUE,
    png_path = png_path,
    artifact_id = sanitize_chat_text_scalar(as.character(
      result$artifact_id %||% NA_character_
    )[[1]]),
    artifact_label = sanitize_chat_text_scalar(as.character(
      result$artifact_label %||% NA_character_
    )[[1]])
  )
}

extract_turn_tool_results <- function(turn) {
  contents <- tryCatch(turn@contents, error = function(e) list())
  if (!is.list(contents) || length(contents) == 0) {
    return(list())
  }

  tool_results <- list()

  for (content in contents) {
    value <- tryCatch(content@value, error = function(e) NULL)

    if (is.null(value) && is.list(content) && !is.null(content$value)) {
      value <- content$value
    }

    if (is.null(value)) {
      next
    }

    unwrapped <- unwrap_tool_result(value)
    if (is.list(unwrapped)) {
      tool_results <- c(tool_results, list(unwrapped))
    }
  }

  tool_results
}

get_turn_text <- function(turn) {
  text_parts <- extract_turn_text_parts(turn)
  if (length(text_parts) == 0) {
    return("")
  }

  paste(text_parts, collapse = "\n\n")
}

get_user_turns <- function(entry) {
  turns <- entry$client$get_turns()
  turns[vapply(
    turns,
    function(turn) {
      identical(
        tryCatch(turn@role, error = function(e) NA_character_),
        "user"
      )
    },
    logical(1)
  )]
}

get_assistant_turn_count <- function(entry) {
  turns <- entry$client$get_turns()
  sum(vapply(
    turns,
    function(turn) {
      identical(
        tryCatch(turn@role, error = function(e) NA_character_),
        "assistant"
      )
    },
    logical(1)
  ))
}

has_user_input <- function(entry) {
  length(get_user_turns(entry)) > 0
}

first_user_preview <- function(entry, width = 90) {
  user_turns <- get_user_turns(entry)
  if (length(user_turns) == 0) {
    return("(no user input)")
  }

  text <- trimws(get_turn_text(user_turns[[1]]))
  if (!nzchar(text)) {
    return("(no user input)")
  }

  if (nchar(text) > width) {
    paste0(substr(text, 1, width - 1), "…")
  } else {
    text
  }
}

turns_fingerprint <- function(entry) {
  digest::digest(entry$client$get_turns(), algo = "md5")
}

thread_display_name <- function(entry) {
  mode_history <- entry$mode_history %||% NULL
  base_name <- if (!is.null(mode_history) && length(mode_history) > 0) {
    paste(as.character(mode_history), collapse = "+")
  } else {
    entry$thread_base_name %||% entry$prompt_name %||% entry$prompt_id
  }
  resume_index <- as.integer(entry$resume_index %||% 0)

  if (!is.na(resume_index) && resume_index > 0) {
    return(paste0(base_name, " (resume", resume_index, ")"))
  }

  base_name
}

entry_last_message_time <- function(entry) {
  entry$last_message_at %||% entry$archived_at %||% entry$created_at
}

should_archive_entry <- function(entry) {
  if (!has_user_input(entry)) {
    return(FALSE)
  }

  source_fingerprint <- entry$resume_source_fingerprint %||% NULL
  if (is.null(source_fingerprint)) {
    return(TRUE)
  }

  !identical(turns_fingerprint(entry), source_fingerprint)
}

format_timestamp <- function(x) {
  format(x, "%Y-%m-%d %H:%M")
}

destroy_chat_observers <- function(observers) {
  if (!is.list(observers) || length(observers) == 0) {
    return(invisible(NULL))
  }

  for (observer in observers) {
    if (!is.null(observer)) {
      observer$destroy()
    }
  }

  invisible(NULL)
}

register_subchat_streaming <- function(
  session,
  input,
  user_input_id,
  chat_id,
  chat_client,
  existing_observers = list(),
  busy_message = "Please wait for the current response to finish.",
  chat_error_prefix = "Chat error: ",
  stream_error_prefix = "Stream error: "
) {
  destroy_chat_observers(existing_observers)

  is_pending <- shiny::reactiveVal(FALSE)

  input_observer <- shiny::observeEvent(
    input[[user_input_id]],
    {
      user_input <- sanitize_chat_text_scalar(input[[user_input_id]])
      shiny::req(!is.na(user_input))

      if (isTRUE(shiny::isolate(is_pending()))) {
        shiny::showNotification(
          busy_message,
          type = "message",
          duration = 2
        )
        return(invisible(NULL))
      }

      response_stream <- tryCatch(
        chat_client$stream_async(
          user_input,
          tool_mode = "sequential",
          stream = "content"
        ),
        error = function(e) {
          is_pending(FALSE)
          shiny::showNotification(
            paste0(chat_error_prefix, conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      shiny::req(!is.null(response_stream))
      is_pending(TRUE)

      append_promise <- tryCatch(
        chat_append(chat_id, response_stream),
        error = function(e) {
          is_pending(FALSE)
          shiny::showNotification(
            paste0(stream_error_prefix, conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      shiny::req(!is.null(append_promise))

      promises::then(
        append_promise,
        onFulfilled = function(...) {
          is_pending(FALSE)
          invisible(NULL)
        },
        onRejected = function(e) {
          is_pending(FALSE)
          shiny::showNotification(
            paste0(stream_error_prefix, conditionMessage(e)),
            type = "error"
          )

          invisible(NULL)
        }
      )
    },
    ignoreInit = TRUE
  )

  list(input_observer)
}
