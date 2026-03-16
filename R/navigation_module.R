# Navigation Module ----

#' Normalize Font Awesome icon names
#'
#' @param name Character scalar icon name
#'
#' @return Character scalar normalized name or NA_character_
#' @export
normalize_fa_icon_name <- function(name) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    return(NA_character_)
  }

  # Support values like "fas fa-database" by extracting the icon token.
  if (grepl("^fa[srb]?\\s+fa-", name)) {
    name <- sub("^fa[srb]?\\s+fa-", "", name)
  }

  aliases <- ICON_ALIASES

  if (name %in% names(aliases)) {
    return(unname(aliases[[name]]))
  }

  name
}

is_valid_fa_icon <- local({
  icon_names <- NULL

  function(name) {
    nm <- normalize_fa_icon_name(name)
    if (is.na(nm)) {
      return(FALSE)
    }

    if (is.null(icon_names)) {
      icon_names <<- tryCatch(
        fontawesome::fa_metadata()$icon_names,
        error = function(...) character()
      )
    }

    if (length(icon_names) == 0) {
      # If metadata is unavailable, avoid false negatives.
      return(TRUE)
    }

    nm %in% icon_names
  }
})

#' Resolve Font Awesome icon with fallback
#'
#' @param primary_icon Character scalar preferred icon
#' @param fallback_icon Character scalar fallback icon
#'
#' @return Character scalar icon name
#' @export
resolve_fa_icon <- function(primary_icon, fallback_icon = ICON_FALLBACK) {
  primary <- normalize_fa_icon_name(primary_icon)
  fallback <- normalize_fa_icon_name(fallback_icon)

  if (!is.na(primary) && is_valid_fa_icon(primary)) {
    return(primary)
  }

  if (!is.na(fallback) && is_valid_fa_icon(fallback)) {
    return(fallback)
  }

  if (is_valid_fa_icon(ICON_FALLBACK)) {
    return(ICON_FALLBACK)
  }

  if (!is.na(fallback)) {
    return(fallback)
  }

  ICON_FALLBACK
}

#' Render page icon from URL or Font Awesome name
#'
#' @param icon_value Character scalar URL or icon name
#' @param alt Character scalar alt text
#' @param fallback_icon Character scalar fallback icon
#'
#' @return Shiny tag
#' @export
render_page_icon <- function(
  icon_value,
  alt = "",
  fallback_icon = ICON_FALLBACK
) {
  fallback_icon <- resolve_fa_icon(fallback_icon, ICON_FALLBACK)

  if (
    !is.character(icon_value) ||
      length(icon_value) != 1 ||
      !nzchar(icon_value)
  ) {
    return(icon(fallback_icon))
  }

  is_image_path <- grepl(
    "^(https?://|/|\\./|\\.\\./|[A-Za-z0-9_-]+/).+\\.(png|svg|gif|ico|webp)(\\?.*)?$",
    icon_value,
    ignore.case = TRUE
  )

  if (is_image_path) {
    fallback_html <- as.character(icon(fallback_icon))
    fallback_html <- gsub("\n", "", fallback_html, fixed = TRUE)
    fallback_html <- gsub("\\\\", "\\\\\\\\", fallback_html)
    fallback_html <- gsub("'", "\\\\'", fallback_html, fixed = TRUE)

    onerror_js <- paste0(
      "this.onerror=null;this.outerHTML='",
      fallback_html,
      "';"
    )

    return(tags$img(
      src = icon_value,
      alt = alt,
      class = "nav-icon-image",
      width = "16",
      height = "16",
      style = "width:1em;height:1em;max-width:1em;max-height:1em;object-fit:contain;vertical-align:text-bottom;",
      onerror = onerror_js
    ))
  }

  icon(resolve_fa_icon(icon_value, fallback_icon))
}

#' Navigation header UI
#'
#' @param app_pages Named list of page definitions
#' @param title Character scalar app title
#'
#' @return Shiny tag
#' @export
navigation_header_ui <- function(app_pages = APP_PAGES, title = "Vasper") {
  hamburger_links <- lapply(names(app_pages), function(key) {
    pg <- app_pages[[key]]
    fallback_icon <- if (
      is.character(pg$icon_fallback) &&
        length(pg$icon_fallback) == 1 &&
        nzchar(pg$icon_fallback)
    ) {
      pg$icon_fallback
    } else {
      "file-alt"
    }

    actionLink(
      paste0("nav_", key),
      label = tagList(
        render_page_icon(pg$icon, pg$title, fallback_icon = fallback_icon),
        pg$title
      ),
      class = "d-block py-1"
    )
  })

  header_links <- c(
    list(
      actionLink(
        "hdr_chat",
        label = tagList(icon("comments"), "Chat"),
        class = "app-header-link"
      )
    ),
    lapply(names(app_pages), function(key) {
      pg <- app_pages[[key]]
      fallback_icon <- if (
        is.character(pg$icon_fallback) &&
          length(pg$icon_fallback) == 1 &&
          nzchar(pg$icon_fallback)
      ) {
        pg$icon_fallback
      } else {
        "file-alt"
      }

      actionLink(
        paste0("hdr_", key),
        label = tagList(
          render_page_icon(pg$icon, pg$title, fallback_icon = fallback_icon),
          pg$title
        ),
        class = "app-header-link"
      )
    })
  )

  div(
    class = "app-header",
    div(
      class = "app-header-main",
      div(class = "app-header-title", title),
      div(
        class = "app-header-links",
        !!!header_links
      )
    ),
    div(
      class = "app-header-menu",
      dropdown(
        actionLink(
          "nav_chat",
          label = tagList(icon("comments"), "Chat"),
          class = "d-block py-1"
        ),
        !!!hamburger_links,
        icon = icon("bars"),
        size = "sm",
        status = "outline-secondary",
        right = TRUE,
        width = "200px"
      )
    )
  )
}

#' Navigation bottom bar UI
#'
#' @return Shiny tag
#' @export
navigation_bottom_bar_ui <- function() {
  div(
    class = "bottom-bar",
    actionLink(
      "scroll_top",
      label = "Top",
      icon = icon("arrow-up")
    ),
    div(
      class = "bottom-bar-center",
      actionLink(
        "toggle_view",
        label = "Hide chat",
        icon = icon("eye-slash"),
        class = "bottom-bar-mid-link"
      ),
      actionLink(
        "clear_chat",
        label = "New chat",
        icon = icon("plus"),
        class = "bottom-bar-mid-link"
      )
    ),
    actionLink(
      "scroll_bottom",
      label = "End",
      icon = icon("arrow-down")
    )
  )
}

#' Navigation server controller
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session
#' @param app_pages Named list of pages
#' @param chat_id Character scalar chat UI id
#' @param on_clear_chat Optional callback invoked when clear chat is requested
#'
#' @return List with navigation helpers/reactives
#' @export
navigation_server <- function(
  input,
  output,
  session,
  app_pages = APP_PAGES,
  chat_id = "main_chat",
  on_clear_chat = NULL
) {
  active_page <- reactiveVal(NULL) # NULL = chat visible
  last_page <- reactiveVal("reports") # remember last page for toggle
  footer_action_enabled <- reactiveValues(
    clear_chat = TRUE,
    toggle_view = TRUE,
    scroll_top = TRUE,
    scroll_bottom = TRUE
  )

  set_footer_action_enabled <- function(action_id, enabled = TRUE) {
    req(is.character(action_id), length(action_id) == 1, nzchar(action_id))

    footer_action_enabled[[action_id]] <- isTRUE(enabled)

    session$sendCustomMessage(
      "set-action-enabled",
      list(
        id = action_id,
        enabled = isTRUE(enabled)
      )
    )

    invisible(isTRUE(enabled))
  }

  is_footer_action_enabled <- function(action_id) {
    value <- footer_action_enabled[[action_id]]
    if (is.null(value)) {
      return(TRUE)
    }
    isTRUE(value)
  }

  set_active_nav <- function(page = c("chat", names(app_pages))) {
    page <- match.arg(page)
    session$sendCustomMessage("set-active-page", list(page = page))
  }

  navigate_to <- function(page_key) {
    active_page(page_key)
    last_page(page_key)
    nav_select("app_view", "pages")
    nav_select("pages_nav", page_key)
    updateActionButton(
      session,
      "toggle_view",
      label = "Show chat",
      icon = icon("comments")
    )
    set_active_nav(page_key)
  }

  navigate_to_chat <- function() {
    active_page(NULL)
    set_active_nav("chat")
    session$sendCustomMessage(
      "scroll-chat-bottom",
      list(id = chat_id, phase = "pre")
    )
    nav_select("app_view", "chat")
    session$onFlushed(
      function() {
        session$sendCustomMessage(
          "scroll-chat-bottom",
          list(id = chat_id, phase = "after")
        )
      },
      once = TRUE
    )
    updateActionButton(
      session,
      "toggle_view",
      label = "Hide chat",
      icon = icon("eye-slash")
    )
  }

  session$onFlushed(
    function() {
      set_active_nav("chat")
    },
    once = TRUE
  )

  scroll_active_content <- function(edge = c("top", "bottom")) {
    edge <- match.arg(edge)

    if (is.null(active_page())) {
      if (edge == "bottom") {
        session$sendCustomMessage(
          "scroll-chat-bottom",
          list(id = chat_id, phase = "after")
        )
      } else {
        session$sendCustomMessage(
          "scroll-to-edge",
          list(id = chat_id, edge = edge)
        )
      }
      return(invisible(NULL))
    }

    session$sendCustomMessage(
      "scroll-to-edge",
      list(selector = "#app_view .tab-pane.active", edge = edge)
    )

    invisible(NULL)
  }

  observeEvent(input$toggle_view, {
    if (!is_footer_action_enabled("toggle_view")) {
      return(invisible(NULL))
    }

    if (is.null(active_page())) {
      navigate_to(last_page())
    } else {
      navigate_to_chat()
    }
  })

  observeEvent(input$scroll_top, {
    if (!is_footer_action_enabled("scroll_top")) {
      return(invisible(NULL))
    }

    scroll_active_content("top")
  })

  observeEvent(input$scroll_bottom, {
    if (!is_footer_action_enabled("scroll_bottom")) {
      return(invisible(NULL))
    }

    scroll_active_content("bottom")
  })

  observeEvent(input$clear_chat, {
    if (!is_footer_action_enabled("clear_chat")) {
      return(invisible(NULL))
    }

    if (is.function(on_clear_chat)) {
      on_clear_chat()
    }
    navigate_to_chat()
  })

  observeEvent(input$nav_chat, {
    navigate_to_chat()
  })

  observeEvent(input$hdr_chat, {
    navigate_to_chat()
  })

  lapply(names(app_pages), function(key) {
    observeEvent(input[[paste0("nav_", key)]], {
      navigate_to(key)
    })
  })

  lapply(names(app_pages), function(key) {
    observeEvent(input[[paste0("hdr_", key)]], {
      navigate_to(key)
    })
  })

  observeEvent(input$client_nav_request, {
    req(input$client_nav_request$page)
    page <- input$client_nav_request$page

    if (identical(page, "chat")) {
      navigate_to_chat()
      return(invisible(NULL))
    }

    if (page %in% names(app_pages)) {
      navigate_to(page)
    }

    invisible(NULL)
  })

  all_pages <- c("chat", names(app_pages))
  show_page <- tool(
    function(page) {
      if (page == "chat") {
        navigate_to_chat()
        return("Navigated to Chat")
      }

      if (!page %in% names(app_pages)) {
        return(paste(
          "Invalid page. Choose one of:",
          paste(all_pages, collapse = ", ")
        ))
      }

      navigate_to(page)
      paste("Navigated to", app_pages[[page]]$title)
    },
    name = "show_page",
    description = paste(
      "Navigate the app UI to a specific page or back to the chat.",
      "Available pages:",
      paste(
        c(
          "chat (Chat)",
          vapply(
            names(app_pages),
            function(k) {
              paste0(k, " (", app_pages[[k]]$title, ")")
            },
            character(1)
          )
        ),
        collapse = ", "
      )
    ),
    arguments = list(
      page = type_enum(
        "The page to navigate to",
        values = all_pages
      )
    )
  )

  list(
    active_page = reactive(active_page()),
    last_page = reactive(last_page()),
    navigate_to = navigate_to,
    navigate_to_chat = navigate_to_chat,
    show_page_tool = show_page,
    set_action_enabled = set_footer_action_enabled,
    is_action_enabled = is_footer_action_enabled
  )
}
