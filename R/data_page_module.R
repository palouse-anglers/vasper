# Data Page Module ----

#' Single data viewer module UI
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @export
data_view_module_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("module_ui"))
}

#' Single data viewer module server
#'
#' @param id Module id
#' @param con DBI connection
#' @param tables_r Reactive expression that returns table names
#' @param metadata_r Reactive expression that returns table metadata
#' @param refresh_nonce_r Reactive expression used to refresh data
#' @param remove_cb Callback used to remove this module
#' @param on_select_table_cb Callback when selected table changes
#' @param initial_table Optional initial table name for this module
#'
#' @return Invisibly TRUE
#' @export
data_view_module_server <- function(
  id,
  con,
  tables_r,
  metadata_r,
  refresh_nonce_r,
  remove_cb,
  on_select_table_cb,
  initial_table = NULL
) {
  moduleServer(id, function(input, output, session) {
    selected_table <- reactiveVal(initial_table)
    collapsed <- reactiveVal(TRUE)

    table_label <- reactive({
      tbl <- selected_table()
      req(tbl)

      md <- metadata_r()
      req(nrow(md) > 0)

      match_idx <- match(tbl, md$table_name)
      if (is.na(match_idx)) {
        return(stringr::str_to_title(gsub("_", " ", tbl)))
      }

      md$table_label[[match_idx]]
    })

    observe({
      tables <- tables_r()
      req(length(tables) > 0)

      current <- selected_table()
      if (is.null(current) || !current %in% tables) {
        default_table <- tables[[1]]
        selected_table(default_table)
      }

      updateSelectInput(
        session,
        "table_name",
        choices = tables,
        selected = selected_table()
      )
    })

    observeEvent(
      input$table_name,
      {
        req(input$table_name)
        selected_table(input$table_name)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      selected_table(),
      {
        req(selected_table())
        on_select_table_cb(selected_table())
      },
      ignoreInit = FALSE
    )

    observeEvent(input$toggle_collapse, {
      collapsed(!collapsed())
    })

    observeEvent(input$delete_module, {
      remove_cb()
    })

    output$table_preview <- renderDT(
      {
        refresh_nonce_r()

        tbl <- selected_table()
        req(tbl)

        escaped_tbl <- as.character(DBI::dbQuoteIdentifier(con, tbl))

        dat <- DBI::dbGetQuery(
          con,
          paste0("SELECT * FROM ", escaped_tbl, " LIMIT 500")
        )

        dat
      },
      options = list(pageLength = 15, scrollX = TRUE)
    )

    output$module_ui <- renderUI({
      selected <- selected_table()
      selected_title <- if (is.null(selected)) "No table selected" else selected

      summary_row_collapsed <- div(
        class = "data-module-summary",
        span(class = "data-module-title", selected_title),
        tags$span(
          class = "badge text-bg-light data-module-label",
          if (!is.null(selected)) table_label() else "No label"
        )
      )

      summary_row_expanded <- div(
        class = "data-module-summary",
        span(class = "data-module-title", selected_title),
        tags$span(
          class = "badge text-bg-light data-module-label",
          if (!is.null(selected)) table_label() else "No label"
        )
      )

      controls <- div(
        class = "data-module-controls",
        actionButton(
          session$ns("toggle_collapse"),
          label = NULL,
          icon = icon(if (collapsed()) "chevron-down" else "chevron-up"),
          class = "btn btn-sm btn-outline-secondary",
          title = if (collapsed()) "Expand" else "Collapse"
        ),
        actionButton(
          session$ns("delete_module"),
          label = NULL,
          icon = icon("trash"),
          class = "btn btn-sm btn-outline-danger",
          title = "Delete"
        )
      )

      if (collapsed()) {
        return(
          div(
            class = "data-module data-module-collapsed card",
            div(
              class = "card-body py-2 px-3 data-module-header-row",
              summary_row_collapsed,
              controls
            )
          )
        )
      }

      div(
        class = "data-module card",
        div(
          class = "card-header data-module-header-row",
          summary_row_expanded,
          controls
        ),
        div(
          class = "card-body",
          selectInput(
            session$ns("table_name"),
            label = "Table",
            choices = tables_r(),
            selected = selected_table(),
            width = "100%"
          ),
          if (!is.null(selected)) {
            tagList(
              div(
                class = "text-muted small mb-2",
                paste("Label:", table_label())
              ),
              DTOutput(session$ns("table_preview"))
            )
          } else {
            div(
              class = "text-muted small mb-2",
              "Select a table to load data."
            )
          }
        )
      )
    })

    outputOptions(output, "table_preview", suspendWhenHidden = FALSE)

    invisible(TRUE)
  })
}
