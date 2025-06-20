#' Shiny Module for Selecting Columns
#'
#' This shiny module gives the user the option to manually map column names to
#' NACC variable names.
#'
#' @param col_names character vector of column names
#' @param id id to link shiny modules
#'
#' @rdname colSelectModule
#'
#' @export

colSelectUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("assign"), label = "Apply Selections"),
    DT::dataTableOutput(ns("vars_table_output"))
  )
}

#' @rdname colSelectModule
#'
#' @param id id to link shiny modules
#' @param col_names reactive character vector with names to choose from
#' @param data_type reactive value; one of 'wls', 'nacc', 'wadrc'
#' @param default_methods named list. Each entry should be named after a variable. The entry should be a named character vector with to elements: one named 'method' to indicate the standardization method to use, and one name 'version' to indicate the version to use for standardization.
#' @param allow_col_selection logical; if `TRUE` (default), allow user to select which columns should be used for each variable.
#'
#' @export

colSelectServer <- function(
  id,
  col_names,
  default_methods,
  data_type,
  allow_col_selection = T
) {
  stopifnot("data_type must be reactive" = shiny::is.reactive(data_type))
  stopifnot("col_names must be reactive" = shiny::is.reactive(col_names))

  shiny::moduleServer(id, function(input, output, session) {
    ## All variables
    all_vars <- c(
      critical_vars,
      visit_vars,
      birth_vars,
      optional_vars,
      na.omit(unlist(diag_contr_pairs[, c(1, 2, 3)], use.names = F)),
      # Derived vars
      "REYTOTAL",
      "REYAREC"
    )

    ## We set var_group as names. Start with "the rest" (set to "Other")
    names(all_vars) <- rep("Other", length(all_vars))

    ## Demographics
    names(all_vars)[
      all_vars %in% c(critical_vars, "RACE", "HANDEDNESS")
    ] <- "Demographics"

    names(all_vars)[all_vars %in% birth_vars] <- "Age/Birthdate"
    names(all_vars)[all_vars %in% visit_vars] <- "Visit Date"
    names(all_vars)[
      all_vars %in% formalArgs(NpsychBatteryNorms::calculate_fas)
    ] <- "Functional Assessment Score"
    names(all_vars)[
      all_vars %in% names(nacc_var_groups)
    ] <- nacc_var_groups[all_vars[all_vars %in% names(nacc_var_groups)]]
    names(all_vars)[
      all_vars %in%
        c(
          "TRAILARR",
          "TRAILALI",
          "TRAILBRR",
          "TRAILBLI",
          "MOCACLOC",
          "MOCACLOH",
          "MOCACLON",
          "CDRSUM"
        )
    ] <- "General Cognition"
    names(all_vars)[
      all_vars %in%
        c(
          "REY1REC",
          "REY2REC",
          "REY3REC",
          "REY4REC",
          "REY5REC",
          "REYTCOR",
          "REYFPOS",
          "REYTNEG",
          "UDSBENRS",
          "MEMTIME",
          "REYTOTAL",
          "REYAREC"
        )
    ] <- "Memory"
    names(all_vars)[all_vars == "OTRLBLI"] <- "Executive Functioning"
    names(all_vars)[all_vars == "NACCUDSD"] <- "Diagnosis"
    names(all_vars)[
      all_vars %in% unlist(diag_contr_pairs[, c(1, 2, 3)], use.names = F)
    ] <- "Diagnosis"

    ## Set up tibble for data.table with columns
    ##    - Variable: the variables we need to map to columns
    ##    - var_group: factor used to group variables in output
    ##    - Required: boolean to indicate if variable must be mapped to continue
    ##    - Column: this holds dropdown menus with column names from to choose from
    ##    - Method: holds dropdown menus to choose standardization methods to use.
    vars_table <- shiny::reactiveVal(value = NULL)
    for_DT <- shiny::reactiveVal(value = NULL)

    shiny::observe({
      shiny::req(data_type())
      shiny::req(col_names())

      tmp_table <- data.table::data.table(
        " " = "",
        Variable = all_vars,
        var_group = factor(
          names(all_vars),
          levels = c(
            "Demographics",
            "Age/Birthdate",
            "Visit Date",
            unique(nacc_var_groups),
            "Functional Assessment Score",
            "Other",
            "Diagnosis"
          )
        ),
        Required = all_vars %in% critical_vars,
        "Short Descriptor" = lapply(
          all_vars,
          \(x) NpsychBatteryNorms::rdd[[x]]$short_descriptor
        ),
        Column = unlist(lapply(
          # purrr::map2_chr(
          all_vars, # .data$Required,
          # c(critical_vars, visit_vars, optional_vars), \(x)
          \(x) {
            y <- x %in% critical_vars
            out <- c("(blank)", "SELECT COLUMN")[as.numeric(y) + 1]

            if (data_type() == "nacc") {
              if (x %in% col_names()) {
                out <- x
              } else {
                if (x %in% c("REYTOTAL", "REYAREC")) {
                  out <- "(CALCULATED)"
                }
              }
            }

            if (data_type() == "wls") {
              if (nacc_to_wls[x] %in% col_names()) {
                out <- unname(nacc_to_wls[x])
              } else {
                if (x %in% c("REYTOTAL", "REYAREC")) {
                  out <- "(CALCULATED)"
                }
              }
            }

            if (data_type() == "wadrc") {
              if (nacc_to_wadrc[x] %in% col_names()) {
                out <- unname(nacc_to_wadrc[x])
              } else {
                if (x %in% c("REYTOTAL", "REYAREC")) {
                  out <- "(CALCULATED)"
                }
              }
            }

            out
          }
        )),
        Method = unlist(lapply(
          all_vars,
          \(x) {
            ## If no method given in default_methods, then we do not give the
            ## option to choose a method.
            if (x %in% names(default_methods)) {
              def_method <- paste0(
                default_methods[[x]]["method"],
                " (",
                default_methods[[x]]["version"],
                ")"
              )

              methods_available <- NpsychBatteryNorms::std_methods(
                var_name = x
              )[
                NpsychBatteryNorms::std_methods(var_name = x)$available == 1,
              ]
            } else {
              return("")
            }

            as.character(
              shiny::selectizeInput(
                inputId = shiny::NS(id, paste0(x, "method")),
                label = NULL,
                choices = with(
                  methods_available,
                  paste0(method, " (", version, ")")
                ),
                selected = def_method
              )
            )
          }
        ))
      )

      tmp_table <- tmp_table[order(tmp_table$var_group)]

      vars_table(tmp_table)
      for_DT(tmp_table)
    }) |>
      shiny::bindEvent(
        col_names(),
        data_type()
      )

    output$vars_table_output <- DT::renderDataTable({
      tmp <- for_DT()

      DT::datatable(
        tmp,
        escape = FALSE,
        rownames = FALSE,
        selection = list(mode = "single", target = "cell"),
        class = list(stripe = FALSE),
        extensions = "RowGroup",
        options = list(
          rowGroup = list(
            # NOTE: 0-indexed... hence -1.
            dataSrc = which(colnames(tmp) == "var_group") - 1
          ),
          columnDefs = list(
            list(
              visible = F,
              targets = which(colnames(tmp) == "var_group") - 1
            ),
            list(
              width = "20px",
              targets = 0
            ),
            list(
              targets = which(colnames(tmp) == "Column") - 1,
              render = if (allow_col_selection) {
                DT::JS(
                  'function (data, type, row, meta) {
                  if (data == "(CALCULATED)") {
                    var cursor=""
                    var text="(CALCULATED)"
                    var background="rgb(226,226,226)"
                  } else {
                    var text=data+`<span style="float: right; font-size: 0.9375rem; padding-top: 0.375rem;" class="glyphicon glyphicon-search" aria-hidden="true"></span>`
                    var cursor="cursor: pointer;"
                    var background="transparent"
                  }
                  return `<div style="font-size: 0.9375rem; background: `+background+`; `+cursor+` position: relative; border-radius: 3px; border-style: solid; border-width: 1px; border-color: #8D959E; padding: 0.375rem; padding-left: 0.75rem; padding-right: 0.75rem;">`+text+`</div>`
                }'
                )
              } else {
                DT::JS(
                  'function (data, type, row, meta) {
                  return `<div style="font-size: 0.9375rem; background:rgb(226, 226, 226); position: relative; border-radius: 3px; border-style: solid; border-width: 1px; border-color: #8D959E; padding: 0.375rem; padding-left: 0.75rem; padding-right: 0.75rem;">`+data+`</div>`
                }'
                )
              }
            )
          ),
          autoWidth = TRUE,
          paging = FALSE,
          info = FALSE,
          dom = "t",
          ordering = FALSE,
          preDrawCallback = DT::JS(
            "function() { Shiny.unbindAll(this.api().table().node()); }"
          ),
          drawCallback = DT::JS(
            paste0(
              'function() {
                  Shiny.bindAll(this.api().table().node());
                  // Set input$varstableDrawn to "on"
                  Shiny.setInputValue("',
              shiny::NS(id, "varstableDrawn"),
              '", "on");
                }'
            )
          )
        )
      ) #|>
      # DT::formatStyle(
      #  columns = which(colnames(tmp) == "Column"),
      # cursor = "pointer" # ,
      # border = "1px", # solid #8D959E;",
      # "border-style" = "solid",
      # "border-width" = "1px",
      # "border-color" = "#8D959E",
      # "padding-top" = "0.375rem",
      # "padding-bottom" = "0.375rem",
      # "border-radius" = "3px",
      # margin = "5px",
      # position = "relative",
      # target = "cell" ,
      # color = "red"
      # )
    })

    vars_table_output_proxy <- DT::dataTableProxy("vars_table_output")

    ## Modal to show when column clicked to choose different column to use.
    output$modal_ui <- shiny::renderUI({
      shiny::selectizeInput(
        inputId = shiny::NS(id, "newColumn"),
        label = NULL,
        choices = "(blank)",
        selected = "(blank)",
        options = list(
          onInitialize = I(paste0(
            "function() {",
            # 'console.log("Here...");',
            'Shiny.setInputValue("',
            shiny::NS(id, "selectizeInputUpdated"),
            '", "on", {priority: "event"});',
            "}"
          ))
        )
      )
    })

    ## Update dropdown menu when the column "Column" is clicked
    shiny::observe({
      shiny::req(input$vars_table_output_cells_selected)

      row_clicked <- input$vars_table_output_cells_selected[1, 1]
      col_clicked <- input$vars_table_output_cells_selected[1, 2]

      if (
        col_clicked == 5 &
          vars_table()$Column[row_clicked] != "(CALCULATED)" &
          allow_col_selection
      ) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::uiOutput(shiny::NS(id, "modal_ui")),
            footer = bslib::layout_columns(
              shiny::actionButton(
                inputId = shiny::NS(id, "change_column"),
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
      shiny::bindEvent(input$vars_table_output_cells_selected)

    ## When dropdown is initialized, update label, choices, and selected:
    shiny::observe({
      shiny::req(input$selectizeInputUpdated)

      row_clicked <- input$vars_table_output_cells_selected[1, 1]

      shiny::updateSelectizeInput(
        inputId = "newColumn",
        label = paste(
          "Choose column to use for ",
          vars_table()$Variable[row_clicked]
        ),
        choices = c(
          c("(blank)", "SELECT COLUMN")[
            as.numeric(vars_table()$Required[row_clicked]) + 1
          ],
          col_names()
        ),
        selected = vars_table()$Column[row_clicked],
        server = T
      )
    }) |>
      shiny::bindEvent(input$selectizeInputUpdated)

    ## Change column when input$change_column is clicked
    shiny::observe({
      tmp <- vars_table()

      if (
        input$newColumn !=
          tmp$Column[input$vars_table_output_cells_selected[1, 1]]
      ) {
        if (
          input$newColumn %in%
            tmp$Column &&
            !input$newColumn %in% c("SELECT COLUMN", "(blank)")
        ) {
          shiny::showNotification(
            shiny::p(
              paste0(
                input$newColumn,
                " was used for ",
                tmp$Variable[tmp$Column == input$newColumn],
                ". It has been removed from that assignment."
              )
            ),
            duration = NULL,
            type = "error",
            closeButton = TRUE
          )
          tmp$Column[tmp$Column == input$newColumn] <- ifelse(
            tmp$Required[tmp$Column == input$newColumn],
            "SELECT COLUMN",
            "(blank)"
          )
        }

        tmp$Column[input$vars_table_output_cells_selected[
          1,
          1
        ]] <- input$newColumn

        vars_table(tmp)

        DT::replaceData(
          vars_table_output_proxy,
          data = tmp,
          rownames = FALSE
        )
      }
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$change_column)

    ## When table first completed, check if enough variables have been identified to move on.
    shiny::observe({
      ## Create a named vector with entries corresponding to "Column" and
      ## names corresponding to "Variable" such that all_input_cols["SEX"]
      ## gives column in data to use for SEX
      all_input_cols <- setNames(
        vars_table()$Column,
        vars_table()$Variable
      )

      ## If all critical variables have been assigned a column...
      if (all(all_input_cols[critical_vars] %in% col_names())) {
        ## ... and EITHER visit date or visit year+mo+day.
        if (
          !all_input_cols["VISITDATE"] %in% c("(blank)", "") |
            !all(
              all_input_cols[c("VISITYR", "VISITMO", "VISITDAY")] %in%
                c("(blank)", "")
            )
        ) {
          ## Next, get vector of variables that have been assigned columns
          vars_found <- names(all_input_cols[which(
            all_input_cols != "(blank)"
          )])

          ## Check if there are methods available for the variables automatically detected
          methods_avail <- sapply(vars_found, \(x) {
            sum(NpsychBatteryNorms::std_methods(var_name = x)$available)
          })

          ## If sum is greater than 0, this means some variables with methods available
          ## have been found, and so we move on automatically.
          if (sum(methods_avail > 0) > 0) {
            # Use NS since this is passed to JS, and not aware of module
            session$sendCustomMessage("click", shiny::NS(id, "assign"))
            session$sendCustomMessage("click", "moveToTables")
          }
        }
      }
    }) |>
      shiny::bindEvent(input$varstableDrawn)

    var_cols <- shiny::reactiveVal()
    std_methods <- shiny::reactiveVal()

    shiny::observe({
      # print(paste0("Assign clicked... (", input$assign, ")"))

      var_cols_tmp <- setNames(vars_table()$Column, vars_table()$Variable)
      var_cols_tmp <- var_cols_tmp[-which(var_cols_tmp %in% "(blank)")]

      std_methods_tmp <- setNames(
        as.list(rep(NA, length(var_cols_tmp))),
        nm = names(var_cols_tmp)
      )

      var_cols_tmp <- var_cols_tmp[-which(var_cols_tmp %in% "(CALCULATED)")]

      std_methods_tmp[which(std_methods_tmp == "(CALCULATED)")] <- names(
        std_methods_tmp
      )[which(std_methods_tmp == "(CALCULATED)")]

      for (i in names(std_methods_tmp)) {
        if (paste0(i, "method") %in% names(input)) {
          std_methods_tmp[[i]] <- setNames(
            gsub(
              pattern = "\\(|\\)",
              replacement = "",
              x = strsplit(input[[paste0(i, "method")]], split = " ")[[1]]
            ),
            c("method", "version")
          )
        }
      }

      std_methods_tmp <- std_methods_tmp[-which(is.na(std_methods_tmp))]

      if (
        "VISITDATE" %in%
          names(var_cols_tmp) &
          any(paste0("VISIT", c("YR", "MO", "DAY")) %in% names(var_cols_tmp))
      ) {
        shiny::showModal(
          shiny::modalDialog(
            "Both VISITDATE and VISITYR/VISITMO/VISITDAY specified. Please choose either or."
          )
        )
      } else {
        if (
          all(c(
            "VISITDATE" %in% names(var_cols_tmp),
            all(paste0("VISIT", c("YR", "MO", "DAY")) %in% names(var_cols_tmp))
          ))
        ) {
          shiny::showModal(
            shiny::modalDialog(
              "Must specify either VISITDATE or all of VISITYR/VISITMO/VISITDAY."
            )
          )
        } else {
          var_cols(var_cols_tmp)
          std_methods(std_methods_tmp)
        }
      }
    }) |>
      shiny::bindEvent(input$assign)

    return(list(var_cols = var_cols, std_methods = std_methods))
  })
}

#' Column selection app
#'
#' @description
#' A short description...
#'
#' @param col_names Column names.
#' @param default_methods Default methods.
#' @param data_type One of `"nacc"`, `"wls"`, or `"wadrc"`.
#' @param allow_col_selection A logical; whether column selection is allowed. If
#'   `TRUE`,
#'
#' @rdname colSelectModule
#'
#' @returns
#' A shiny app.
#'
#' @export

colSelectApp <- function(
  col_names,
  default_methods,
  data_type = c("nacc", "wls", "wadrc"),
  allow_col_selection = T
) {
  ui <- bslib::page_fluid(
    shiny::selectizeInput(
      inputId = "whatever",
      label = NULL,
      choices = c("a", "b", "c")
    ),
    colSelectUI(id = "colselect")
  )

  server <- function(input, output, session) {
    var_cols <- colSelectServer(
      id = "colselect",
      col_names,
      default_methods = default_methods,
      data_type = data_type,
      allow_col_selection = allow_col_selection
    )

    shiny::observe({
      print(var_cols$var_cols())
    })
  }

  shiny::shinyApp(ui, server)
}
