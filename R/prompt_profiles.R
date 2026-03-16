# Prompt Profiles ----

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

trim_blank_lines <- function(lines) {
  out <- lines

  while (length(out) > 0 && !nzchar(trimws(out[[1]]))) {
    out <- out[-1]
  }

  while (length(out) > 0 && !nzchar(trimws(out[[length(out)]]))) {
    out <- out[-length(out)]
  }

  out
}

extract_prompt_section <- function(lines, section_name) {
  start_idx <- grep(
    paste0("^##\\s+", section_name, "\\s*$"),
    lines,
    perl = TRUE
  )

  if (length(start_idx) == 0) {
    return(NULL)
  }

  start <- start_idx[[1]]
  next_headers <- grep("^##\\s+", lines)
  next_headers <- next_headers[next_headers > start]
  end <- if (length(next_headers) > 0) {
    next_headers[[1]] - 1
  } else {
    length(lines)
  }

  body <- trim_blank_lines(lines[(start + 1):end])
  if (length(body) == 0) {
    return(NULL)
  }

  paste(body, collapse = "\n")
}

load_prompt_file_sections <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste0("Missing required prompt file: ", path),
      call. = FALSE
    )
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  system_prompt <- extract_prompt_section(lines, "system_prompt")
  chat_welcome_message <- extract_prompt_section(lines, "chat_welcome_message")

  if (is.null(system_prompt) || !nzchar(trimws(system_prompt))) {
    stop(
      paste0("Missing required '## system_prompt' section in ", path, "."),
      call. = FALSE
    )
  }

  if (is.null(chat_welcome_message) || !nzchar(trimws(chat_welcome_message))) {
    stop(
      paste0(
        "Missing required '## chat_welcome_message' section in ",
        path,
        "."
      ),
      call. = FALSE
    )
  }

  list(
    system_prompt = system_prompt,
    chat_welcome_message = chat_welcome_message
  )
}

validate_tool_policy <- function(policy) {
  mode <- policy$mode %||% "all"
  mode <- tolower(mode)

  if (!mode %in% c("all", "allowlist", "rejectlist")) {
    stop(
      "Invalid tool policy mode. Expected one of: all, allowlist, rejectlist",
      call. = FALSE
    )
  }

  tools <- policy$tools %||% character()
  tools <- as.character(tools)

  list(
    mode = mode,
    tools = unique(tools)
  )
}

resolve_prompt_manifest <- function(
  path = file.path("prompts", "manifest.json")
) {
  if (!file.exists(path)) {
    default_file <- file.path("prompts", "prompt.md")
    sections <- load_prompt_file_sections(default_file)

    return(list(
      default_prompt_id = "default",
      prompts = list(
        list(
          id = "default",
          name = "Default",
          file = default_file,
          tool_policy = list(mode = "all", tools = character()),
          system_prompt = sections$system_prompt,
          chat_welcome_message = sections$chat_welcome_message
        )
      )
    ))
  }

  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  prompts <- manifest$prompts %||% list()
  if (length(prompts) == 0) {
    stop(
      "prompts/manifest.json must define at least one prompt profile.",
      call. = FALSE
    )
  }

  prompts <- lapply(prompts, function(p) {
    id <- as.character(p$id %||% "")
    name <- as.character(p$name %||% id)
    file <- as.character(p$file %||% "")

    if (!nzchar(id) || !nzchar(file)) {
      stop(
        "Each prompt profile must include non-empty 'id' and 'file'.",
        call. = FALSE
      )
    }

    prompt_file <- if (grepl("^prompts[/\\\\]", file)) {
      file
    } else {
      file.path("prompts", file)
    }

    sections <- load_prompt_file_sections(prompt_file)
    policy <- validate_tool_policy(p$tool_policy %||% list(mode = "all"))

    list(
      id = id,
      name = if (nzchar(name)) name else id,
      file = prompt_file,
      tool_policy = policy,
      system_prompt = sections$system_prompt,
      chat_welcome_message = sections$chat_welcome_message
    )
  })

  ids <- vapply(prompts, `[[`, character(1), "id")
  if (anyDuplicated(ids)) {
    stop(
      "Prompt profile ids must be unique in prompts/manifest.json.",
      call. = FALSE
    )
  }

  default_prompt_id <- as.character(manifest$default_prompt_id %||% ids[[1]])
  if (!default_prompt_id %in% ids) {
    stop(
      "default_prompt_id must match one of the prompt profile ids.",
      call. = FALSE
    )
  }

  list(
    default_prompt_id = default_prompt_id,
    prompts = prompts
  )
}

get_prompt_profile <- function(prompt_config, prompt_id) {
  matches <- prompt_config$prompts[
    vapply(
      prompt_config$prompts,
      function(p) identical(p$id, prompt_id),
      logical(1)
    )
  ]

  if (length(matches) == 0) {
    stop(paste0("Unknown prompt profile id: ", prompt_id), call. = FALSE)
  }

  matches[[1]]
}

resolve_prompt_tools <- function(prompt_profile, available_tools) {
  policy <- prompt_profile$tool_policy
  mode <- policy$mode
  selected_names <- switch(
    mode,
    all = names(available_tools),
    allowlist = intersect(policy$tools, names(available_tools)),
    rejectlist = setdiff(names(available_tools), policy$tools)
  )

  available_tools[selected_names]
}

list_allowed_tool_names <- function(prompt_profile, available_tools) {
  names(resolve_prompt_tools(prompt_profile, available_tools))
}

list_rejected_tool_names <- function(prompt_profile, available_tools) {
  setdiff(
    names(available_tools),
    list_allowed_tool_names(prompt_profile, available_tools)
  )
}

render_chat_turns <- function(turns, chat_id) {
  chat_clear(chat_id)

  for (turn in turns) {
    role_value <- normalize_turn_role(turn)
    if (is.na(role_value)) {
      next
    }

    text_parts <- extract_turn_text_parts(turn)
    if (length(text_parts) == 0) {
      next
    }

    chat_append(
      chat_id,
      list(role = role_value, content = paste(text_parts, collapse = "\n\n"))
    )
  }

  invisible(NULL)
}
