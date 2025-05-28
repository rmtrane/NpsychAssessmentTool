descriptionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(
      ns("descriptions")
    ),
    shiny::actionButton(
      inputId = ns("add_row"),
      label = "Add Group"
    )
  )
}

descriptionsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    descriptions <- shiny::reactiveVal(
      data.frame(
        Label = c(
          "Impaired",
          "Borderline",
          "Low Average",
          "Average",
          "High Average",
          "Superior",
          "Very Superior"
        ),
        Upper_Bound = c(0.03, 0.10, 0.26, 0.76, 0.92, 0.97, 1) * 100,
        Color = calc_fill_colors(7),
        # as.character(shiny::icon("remove", lib = "glyphicon"))
        Remove = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation"></i>'
      )
    )

    # descriptions <- reactiveVal(default_descriptions)

    output$descriptions <- DT::renderDataTable({
      DT::datatable(
        descriptions(),
        colnames = c(
          "Upper Bound" = "Upper_Bound",
          " " = "Remove"
        ),
        options = list(
          ordering = F,
          dom = "t",
          columnDefs = list(
            list(
              targets = 1,
              render = DT::JS(
                'function(data, type, row, meta) {
                  return data + `%`
                }'
              )
            ),
            list(
              targets = 2,
              render = DT::JS(
                'function (data, type, row, meta) {
                return `<div style="text-align: center; background:`+data+`">`+data+`</div>`
              }'
              )
            )
          )
        ),
        rownames = F,
        editable = list(target = "cell", disable = list(columns = 2)),
        selection = "none",
        #   list(
        #   target = "row",
        #   mode = "single"
        # ),
        escape = F
      )
    })

    descriptions_proxy <- DT::dataTableProxy("descriptions")

    shiny::observe({
      shiny::req(input$descriptions_cell_clicked$col)

      row <- input$descriptions_cell_clicked$row
      col <- input$descriptions_cell_clicked$col

      if (col == 3) {
        tmp <- descriptions()
        tmp <- tmp[-row, ]

        DT::replaceData(descriptions_proxy, data = tmp, rownames = F)

        descriptions(tmp)
      }

      if (col == 2) {
        shiny::showModal(
          shiny::modalDialog(
            shinyWidgets::colorPickr(
              inputId = shiny::NS(id, "new_color"),
              label = "",
              selected = descriptions()$Color[row]
            ),
            footer = bslib::layout_columns(
              shiny::actionButton(
                inputId = shiny::NS(id, "update_color"),
                label = "Use"
              ),
              shiny::modalButton(
                label = "Dismiss"
              ),
              col_widths = c(3, -6, 3)
            ),
            easyClose = T
          )
        )
      }
    }) |>
      shiny::bindEvent(
        input$descriptions_cell_clicked
      )

    shiny::observe({
      tmp <- descriptions()

      tmp$Color[
        input$descriptions_cell_clicked$row
      ] <- input$new_color

      DT::replaceData(
        descriptions_proxy,
        data = tmp,
        rownames = F
      )

      descriptions(tmp)

      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$update_color)

    shiny::observe({
      tmp <- DT::editData(
        descriptions(),
        input$descriptions_cell_edit,
        rownames = F
      )

      tmp <- tmp[order(tmp$Upper_Bound), ]

      DT::replaceData(
        descriptions_proxy,
        tmp,
        rownames = F
      )

      descriptions(tmp)
    }) |>
      shiny::bindEvent(
        input$descriptions_cell_edit
      )

    shiny::observe({
      tmp <- rbind(
        descriptions(),
        data.frame(
          "Label" = "New Group",
          "Upper_Bound" = Inf,
          "Color" = "#FFFFFF",
          "Remove" = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation"></i>'
        )
      )

      DT::replaceData(descriptions_proxy, data = tmp, rownames = FALSE)

      descriptions(tmp)
    }) |>
      shiny::bindEvent(
        input$add_row
      )

    desc_out <- shiny::reactiveVal()
    fill_out <- shiny::reactiveVal()

    shiny::observe({
      desc_out(with(descriptions(), setNames(Upper_Bound / 100, Label)))
      fill_out(with(descriptions(), setNames(Color, Label)))
    })

    return(
      list(
        fill_values = fill_out,
        descriptions = desc_out
      )
    )
  })
}

descriptionsApp <- function() {
  ui <- bslib::page_fluid(descriptionsUI("desc"))

  server <- function(input, output, session) descriptionsServer("desc")

  shiny::shinyApp(ui, server)
}
