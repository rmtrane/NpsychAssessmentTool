#' Shiny Module to Display Longitudinal Trends in Table
#'
#' Table version of the plotCogVarModule.
#'
#' @param id string to tie UI to Server
#' @inheritParams assessment_longitudinal_table
#'
#' @rdname longTableModule
#'
#' @export
longTableUI <- function(id) {
  shiny::tagList(
    # gt::gt_output(shiny::NS(id, "long_table"))
    shiny::uiOutput(shiny::NS(id, "long_table"))
    # shiny::actionButton(
    #   shiny::NS(id, "show_hide_empty"),
    #   label = "Hide Visits with No Scores"
    # )
  )
}

#' @rdname longTableModule
#'
#' @param print_updating logical (default `FALSE`); should message be printed to let user know the table is updating. For debugging.
#'
#' @export
longTableServer <- function(
  id,
  dat,
  # descriptions = ,
  methods = "infer",
  table_font_size = shiny::reactive(100),
  print_updating = F
) {
  shiny::moduleServer(id, function(input, output, session) {
    all_visits <- shiny::reactiveVal(value = TRUE)

    shiny::observe({
      all_visits(!all_visits())

      shiny::updateActionButton(
        inputId = "show_hide_empty",
        label = c("Show All Visits", "Hide Visits with No Scores")[
          all_visits() + 1
        ]
      )
    }) |>
      shiny::bindEvent(input$show_hide_empty)

    output$long_table <- shiny::renderUI({
      if (print_updating) {
        print("Updating longitudinal table...")
      }

      assessment_longitudinal_table(
        dat(),
        methods = methods,
        table_font_size = table_font_size(),
        table_id = "long_table",
        show_all_visits = all_visits(),
        stubhead_label = shiny::HTML(
          paste0(
            '<button id="',
            shiny::NS(id, "show_hide_empty"),
            '" type="button"',
            'class="btn btn-default action-button"',
            'style="height: 16px; line-height: 80%; padding-top: 7px; padding-bottom: 16px; width: 250px">',
            'Hide Visits with No Scores',
            '</button>'
          )
        )
      )
    })
  })
}

longTableApp <- function(dat) {
  ui <- bslib::page_fillable(
    longTableUI("long_table")
  )

  server <- function(input, output, session) {
    longTableServer("long_table", shiny::reactive(dat), methods = "infer")
  }

  shiny::shinyApp(ui, server)
}
