# Data Page Manager ----

#' Data page manager UI
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @export
data_page_manager_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "data-page",
    div(
      class = "data-page-toolbar",
      h5(class = "mb-0", "Data"),
      actionButton(
        ns("open_add_views"),
        label = "Add views",
        icon = icon("plus"),
        class = "btn btn-sm btn-outline-primary"
      )
    ),
    uiOutput(ns("modules_ui"))
  )
}

#' Data page manager server
#'
#' @param id Module id
#' @param con DBI connection
#' @param refresh_nonce_r Reactive expression used to refresh table lists/data
#' @param include_tables Table names to force include
#' @param ignore_tables Table names to ignore
#'
#' @return Invisibly TRUE
#' @export
data_page_manager_server <- function(
  id,
  con,
  refresh_nonce_r,
  include_tables = c("table_metadata"),
  ignore_tables = character()
) {
  moduleServer(id, function(input, output, session) {
    module_specs <- reactiveVal(list())
    module_counter <- reactiveVal(0)
    started_module_ids <- reactiveVal(character())
    selected_tables <- reactiveVal(list())

    format_table_choice <- function(
      table_name,
      table_label,
      row_count,
      column_count
    ) {
      dim_label <- paste0(
        ifelse(is.na(row_count), "?", format(row_count, big.mark = ",")),
        " rows × ",
        ifelse(is.na(column_count), "?", column_count),
        " cols"
      )

      tags$div(
        class = "data-table-choice",
        tags$div(tags$code(table_name)),
        tags$div(class = "small text-muted", table_label),
        tags$div(class = "small text-muted", dim_label)
      )
    }

    tables_r <- reactive({
      refresh_nonce_r()
      list_data_tables(
        con = con,
        include_tables = include_tables,
        ignore_tables = ignore_tables
      )
    })

    metadata_r <- reactive({
      refresh_nonce_r()
      get_table_metadata(
        con = con,
        include_tables = include_tables,
        ignore_tables = ignore_tables
      )
    })

    remove_module <- function(module_id) {
      specs <- module_specs()
      keep <- purrr::map_lgl(specs, ~ !identical(.x$id, module_id))
      specs <- specs[keep]

      module_specs(specs)

      current <- selected_tables()
      current[[module_id]] <- NULL
      selected_tables(current)
      invisible(NULL)
    }

    register_selected_table <- function(module_id, table_name) {
      req(nzchar(table_name))

      current <- selected_tables()
      current[[module_id]] <- table_name
      selected_tables(current)

      invisible(NULL)
    }

    get_min_table_metadata <- function() {
      md <- isolate(metadata_r())

      if (nrow(md) == 0) {
        return(tibble::tibble(
          table_name = character(),
          table_label = character(),
          row_count = integer(),
          column_count = integer()
        ))
      }

      md |>
        dplyr::transmute(
          table_name,
          table_label,
          row_count,
          column_count
        )
    }

    apply_selected_views <- function(selected) {
      selected <- unique(as.character(selected))
      selected <- selected[nzchar(selected)]

      md <- get_min_table_metadata()
      available <- md$table_name

      unknown_tables <- setdiff(selected, available)
      selected <- intersect(selected, available)

      specs <- isolate(module_specs())
      current_tables <- vapply(
        specs,
        function(x) {
          if (is.null(x$table_name)) NA_character_ else x$table_name
        },
        character(1)
      )

      # Keep only checked current views.
      keep_mask <- !is.na(current_tables) & current_tables %in% selected
      removed_tables <- unique(current_tables[
        !keep_mask & !is.na(current_tables)
      ])
      specs <- specs[keep_mask]

      # Add checked views that are not open yet.
      kept_tables <- vapply(
        specs,
        function(x) {
          if (is.null(x$table_name)) NA_character_ else x$table_name
        },
        character(1)
      )
      to_add <- setdiff(selected, kept_tables)

      for (tbl in to_add) {
        next_idx <- isolate(module_counter()) + 1
        module_counter(next_idx)

        specs[[length(specs) + 1]] <- list(
          id = paste0("view_", next_idx),
          table_name = tbl
        )
      }

      module_specs(specs)

      # Keep selected table map aligned for modal defaults.
      table_map <- lapply(specs, function(x) x$table_name)
      names(table_map) <- vapply(specs, function(x) x$id, character(1))
      selected_tables(table_map)

      list(
        selected_tables = selected,
        added_tables = to_add,
        removed_tables = removed_tables,
        unknown_tables = unknown_tables
      )
    }

    get_selected_table_names <- function() {
      unique(unlist(isolate(selected_tables()), use.names = FALSE))
    }

    add_selected_views <- function(table_names) {
      table_names <- table_names |>
        unlist(recursive = TRUE, use.names = FALSE) |>
        as.character() |>
        trimws()
      table_names <- unique(table_names[nzchar(table_names)])

      combined <- unique(c(get_selected_table_names(), table_names))
      apply_selected_views(combined)
    }

    observeEvent(input$open_add_views, {
      md <- metadata_r()

      if (nrow(md) == 0) {
        showNotification("No tables available to add.", type = "warning")
        return(invisible(NULL))
      }

      current_selected <- unlist(selected_tables(), use.names = FALSE)

      choice_names <- lapply(seq_len(nrow(md)), function(i) {
        format_table_choice(
          table_name = md$table_name[[i]],
          table_label = md$table_label[[i]],
          row_count = md$row_count[[i]],
          column_count = md$column_count[[i]]
        )
      })

      showModal(
        modalDialog(
          title = "Manage data views",
          checkboxGroupInput(
            session$ns("add_tables"),
            label = "Select table(s)",
            choiceNames = choice_names,
            choiceValues = md$table_name,
            selected = intersect(current_selected, md$table_name),
            width = "100%"
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirm_add_views"),
              label = "Apply",
              class = "btn btn-primary"
            )
          ),
          size = "s"
        )
      )

      invisible(NULL)
    })

    observeEvent(input$confirm_add_views, {
      selected <- input$add_tables
      if (is.null(selected)) {
        selected <- character()
      }

      apply_selected_views(selected)
      removeModal()
      invisible(NULL)
    })

    observe({
      specs <- module_specs()
      ids <- purrr::map_chr(specs, "id")
      known <- started_module_ids()
      new_ids <- setdiff(ids, known)

      purrr::walk(new_ids, function(module_id) {
        spec <- purrr::detect(specs, ~ identical(.x$id, module_id))

        data_view_module_server(
          id = module_id,
          con = con,
          tables_r = tables_r,
          metadata_r = metadata_r,
          refresh_nonce_r = refresh_nonce_r,
          remove_cb = function() remove_module(module_id),
          on_select_table_cb = function(table_name) {
            register_selected_table(module_id, table_name)
          },
          initial_table = spec$table_name
        )
      })

      started_module_ids(union(known, new_ids))
    })

    output$modules_ui <- renderUI({
      specs <- module_specs()

      if (length(specs) == 0) {
        return(div(
          class = "text-muted",
          "No data views yet. Use 'Add views' to create one or more."
        ))
      }

      tagList(
        lapply(specs, function(spec) {
          data_view_module_ui(
            id = session$ns(spec$id)
          )
        })
      )
    })

    list(
      get_metadata = get_min_table_metadata,
      set_views = apply_selected_views,
      get_selected_tables = get_selected_table_names,
      add_views = add_selected_views
    )
  })
}
