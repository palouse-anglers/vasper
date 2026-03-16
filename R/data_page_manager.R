# Data Page Manager ----

TABLE_GROUP_CONFIG <- list(
  query_tables = list(label = "Query tables", rank = 1L),
  api_query_tables = list(label = "API Query tables", rank = 2L),
  built_in_tables = list(label = "Built-in tables", rank = 3L)
)

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
        label = "Manage views",
        icon = icon("table-columns"),
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

    startup_tables <- unique(c(
      unname(unlist(TABLE_NAMES, use.names = FALSE)),
      "table_metadata"
    ))

    table_group_label <- function(group_id) {
      TABLE_GROUP_CONFIG[[group_id]]$label
    }

    table_group_rank <- function(group_id) {
      TABLE_GROUP_CONFIG[[group_id]]$rank
    }

    table_group_id <- function(table_name, source = NA_character_) {
      if (table_name %in% startup_tables) {
        return("built_in_tables")
      }

      if (identical(source, "query_tables")) {
        return("query_tables")
      }

      "api_query_tables"
    }

    classify_table_group <- function(table_name, source = NA_character_) {
      table_group_label(table_group_id(table_name, source))
    }

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
        tags$div(
          class = "data-table-choice-header",
          tags$span(
            class = "data-table-choice-name font-monospace",
            table_name
          ),
          tags$span(
            class = "small text-muted data-table-choice-dims",
            dim_label
          )
        ),
        tags$div(
          class = "small text-muted data-table-choice-meta",
          table_label
        )
      )
    }

    table_group_input_id <- function(group_label) {
      paste0(
        "add_tables_",
        gsub("_+", "_", gsub("[^a-z0-9]+", "_", tolower(group_label)))
      )
    }

    tables_r <- reactive({
      md <- metadata_r()
      if (nrow(md) == 0) {
        return(character())
      }

      md$table_name
    })

    metadata_r <- reactive({
      refresh_nonce_r()

      md <- get_table_metadata(
        con = con,
        include_tables = include_tables,
        ignore_tables = ignore_tables
      )

      if (nrow(md) == 0) {
        return(md)
      }

      md |>
        dplyr::mutate(
          .group_id = purrr::map2_chr(
            table_name,
            source,
            table_group_id
          ),
          table_group = purrr::map2_chr(
            table_name,
            source,
            classify_table_group
          ),
          .group_rank = vapply(.group_id, table_group_rank, integer(1))
        ) |>
        dplyr::arrange(.group_rank, table_label, table_name) |>
        dplyr::select(-.group_id, -.group_rank)
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
          source = character(),
          source_detail = character(),
          row_count = integer(),
          column_count = integer()
        ))
      }

      md |>
        dplyr::transmute(
          table_name,
          table_label,
          source,
          source_detail,
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

      group_sections <- md |>
        dplyr::group_split(table_group, .keep = TRUE) |>
        lapply(function(group_md) {
          group_label <- group_md$table_group[[1]]
          input_id <- table_group_input_id(group_label)

          choice_names <- lapply(seq_len(nrow(group_md)), function(i) {
            format_table_choice(
              table_name = group_md$table_name[[i]],
              table_label = group_md$table_label[[i]],
              row_count = group_md$row_count[[i]],
              column_count = group_md$column_count[[i]]
            )
          })

          checkboxGroupInput(
            session$ns(input_id),
            label = group_label,
            choiceNames = choice_names,
            choiceValues = group_md$table_name,
            selected = intersect(current_selected, group_md$table_name),
            width = "100%"
          )
        })

      showModal(
        modalDialog(
          title = "Manage data views",
          tags$div(class = "small text-muted mb-2", "Select table(s)"),
          tagList(!!!group_sections),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirm_add_views"),
              label = "Apply",
              class = "btn btn-primary"
            )
          ),
          size = "m",
          class = "data-views-modal"
        )
      )

      invisible(NULL)
    })

    observeEvent(input$confirm_add_views, {
      md <- metadata_r()
      groups <- unique(md$table_group)

      selected <- unlist(
        lapply(groups, function(group_label) {
          input[[table_group_input_id(group_label)]] %||% character()
        }),
        use.names = FALSE
      )

      if (length(selected) == 0) {
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
          "No data views yet. Use 'Manage views' to create one or more."
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
      get_metadata = function(include_source = FALSE) {
        md <- get_min_table_metadata()

        if (isTRUE(include_source)) {
          return(md)
        }

        keep <- c("table_name", "table_label", "row_count", "column_count")
        md[, keep, drop = FALSE]
      },
      set_views = apply_selected_views,
      get_selected_tables = get_selected_table_names,
      add_views = add_selected_views
    )
  })
}
