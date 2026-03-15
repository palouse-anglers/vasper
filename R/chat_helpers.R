# Chat Helpers ----

normalize_table_names <- function(x) {
  flat <- unlist(x, recursive = TRUE, use.names = FALSE)
  flat <- as.character(flat)
  flat <- trimws(flat)
  flat <- flat[nzchar(flat)]
  unique(flat)
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

get_turn_text <- function(turn) {
  contents <- tryCatch(turn@contents, error = function(e) list())
  if (!is.list(contents) || length(contents) == 0) {
    return("")
  }

  text_parts <- vapply(
    contents,
    function(content) {
      txt <- tryCatch(content@text, error = function(e) NA_character_)
      if (is.character(txt) && length(txt) == 1 && nzchar(txt)) {
        return(txt)
      }
      ""
    },
    character(1)
  )

  paste(text_parts[nzchar(text_parts)], collapse = "\n\n")
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
