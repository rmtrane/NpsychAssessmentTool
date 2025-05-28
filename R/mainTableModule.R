#' Shiny Module to Display NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' @param id An ID string to match module UI and server UI
#' @param table_font_size (optional) scalar indicating font size as a percentage.
#' @inheritParams assessment_summary_table
#'
#' @rdname mainTableModule
#'
#' @export

mainTableUI <- function(id) {
  bslib::card(bslib::card_body(
    # shiny::actionButton(shiny::NS(id, "updateTable"), "Update"),
    shiny::uiOutput(shiny::NS(id, "mainTable")),
    fillable = F
  ))
}

#' @rdname mainTableModule
#'
#' @param print_updating logical (defualt `FALSE`); should a message be displayed when table is being updated? For debugging.
#'
#' @export
mainTableServer <- function(
  id,
  dat,
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  include_caption = F,
  print_updating = F
) {
  shiny::moduleServer(id, function(input, output, session) {
    # if (!shiny::is.reactive(descriptions)) {
    #   descriptions <- shiny::reactiveVal(descriptions)
    # }

    # if (!shiny::is.reactive(fill_values)) {
    #   fill_values <- shiny::reactiveVal(fill_values)
    # }

    # if (!shiny::is.reactive(table_font_size)) {
    #   table_font_size <- shiny::reactiveVal(table_font_size)
    # }

    output$mainTable <- gt::render_gt({
      for_table <- dat()

      if (is.null(for_table)) {
        return()
      } else {
        stopifnot(
          "'for_table' must be a data.table object" = data.table::is.data.table(
            for_table
          )
        )
      }

      if (nrow(data.frame(for_table)) != 1) {
        return()
      }

      if (print_updating) {
        print("Updating main table...")
      }

      # message("Within mainTable: ", for_table$std_REYTOTAL)

      assessment_summary_table(
        for_table,
        "NACCID",
        descriptions(),
        fill_values(),
        methods,
        include_caption,
        16 * table_font_size() / 100
      ) |>
        gt::tab_options(
          data_row.padding = gt::px(2),
          row_group.padding = gt::px(4),
          table.font.size = gt::pct(table_font_size())
        )
    }) |>
      shiny::bindCache(
        dat(),
        table_font_size(),
        descriptions(),
        fill_values()
      )

    # shiny::observe(
    #   session$sendCustomMessage("removeRawSuffix", "")
    # ) |>
    #   shiny::bindEvent(input$updateTable)
  })
}

#' @rdname mainTableModule
#'
#' @export
mainTableApp <- function(dat) {
  stopifnot(
    "htmltools must be installed to use this function" = rlang::check_installed(
      "htmltools"
    )
  )

  shiny::addResourcePath("www", "inst/shiny/www")

  ui <- bslib::page_fluid(
    htmltools::tags$header(
      htmltools::tags$script(src = "www/scripts.js"),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    shiny::tags$div(id = "spinner", class = "loader"),
    shiny::tags$div(id = "spinner_overlay", class = "loader_overlay"),
    mainTableUI("main_table")
  )

  server <- function(input, output, session) {
    mainTableServer(
      "main_table",
      dat = shiny::reactive(dat),
      methods = "infer",
      table_font_size = 100
    )
  }

  shiny::shinyApp(ui, server)
}
