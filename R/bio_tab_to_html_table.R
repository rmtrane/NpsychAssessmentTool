#' Convert biomarker data table to HTML table
#'
#' @description
#' A short description...
#'
#' @param tab_for_gt A list of `data.table`s, where each `data.table` is named after the Panda table that was queried to get the data.
#' @param densities A list of density data.
#' @param cuts A list or data structure containing cut-off values.
#' @param www_path Path to www/ folder that contains .css and .js files. Defaults to either inst/www or www subdir of NpsychAssessmentTool pacakge.
#' @param print_x For debugging
#'
#' @returns
#' A `shiny::tagList` object representing the HTML table. Will error if `tab_for_gt` is not
#' a list of `data.table`s as expected.
#'
#' @export
bio_tab_to_html_table <- function(
  tab_for_gt,
  densities,
  cuts,
  www_path = if (dir.exists("inst/www")) {
    "inst/www"
  } else {
    system.file("www", package = "NpsychAssessmentTool")
  },
  print_x = FALSE
) {
  ## To avoid notes in R CMD check
  method <- NULL
  name <- NULL

  if (!is.list(tab_for_gt) | inherits(tab_for_gt, "data.table")) {
    cli::cli_abort(
      "{.arg tab_for_gt} must be a list of {.cls data.table}'s named after the Panda table that was queried to get the data."
    )
  }

  tab_for_gt <- tab_for_gt[!unlist(lapply(tab_for_gt, is.null))]

  tab_for_gt <- lapply(tab_for_gt, \(x) {
    if (inherits(x, "try-error")) {
      x <- data.table::data.table(
        name = x[1]
      )
    }

    if (inherits(x, "error-message")) {
      x <- data.table::data.table(name = x)
    }

    x
  })

  ## If some entries are not data.table's, abort.
  if (!all(sapply(tab_for_gt, data.table::is.data.table))) {
    non_dts <- tab_for_gt[!sapply(tab_for_gt, data.table::is.data.table)]

    cli::cli_abort(
      "When {.arg tab_for_gt} is of class {.cls list}, all elements must be of class {.cls data.table}, but {.val {names(non_dts)}} {?is/are} of class {.cls {unlist(sapply(non_dts, class))}}"
    )
  }

  ## Bind tables together with new column giving 'table' from names
  tab_for_gt <- data.table::rbindlist(
    purrr::imap(tab_for_gt, \(x, idx) {
      ## First, add extra row that is simply name of table. This will allow us
      ## to create row group names.
      data.table::rbindlist(
        list(
          data.table::data.table(name = idx),
          x
        ),
        fill = TRUE
      )
    }),
    fill = TRUE,
    idcol = "table"
  )

  ## Get all visit dates
  visit_dates <- setdiff(names(tab_for_gt), c("name", "table"))

  ## Arrange columns by visits
  tab_for_gt <- tab_for_gt[,
    c("table", "name", sort(visit_dates)),
    with = F
  ]

  ## Create method column giving the visit type (LP or PET)
  tab_for_gt[,
    method := data.table::fcase(
      table %in% c("Local Roche CSF - Sarstedt freeze 2, cleaned", "Local Roche CSF - Sarstedt freeze 3", "Local Roche CSF - Sarstedt freeze, cleaned", "NTK MultiObs - CSF analytes", "NTK2 MultiObs - CSF, 20230311", "HDX Plasma - pTau217") ,
      "LP Visits"                                                                                                                                                                                                                               ,
      table %in% c("MK6240_NFT_Rating", "NAV4694 Visual Ratings", "PIB Visual Rating 20180126")                                                                                                                                                 ,
      "PET Visits"                                                                                                                                                                                                                              ,
      default = "Other"
    )
  ]

  ## Get row of ages.
  age_rows <- tab_for_gt[
    tab_for_gt$name == "Age"
  ][,
    table := NULL
  ][,
    setNames(
      nm = c("table", names(.SD)),
      c(
        "age",
        ## Get all unique ages in each column...
        lapply(.SD, \(x) {
          if (is.list(x)) {
            x <- unlist(
              lapply(x, \(y) {
                if (is.null(y)) {
                  return(NA)
                }
                y
              })
            )
          }

          if (all(is.na(x))) {
            return(NA_real_)
          }

          unlist(unique(na.omit(x)))
        })
      )
    ),
    ## ... by method
    by = method
  ]

  ## Make sure column order is correct.
  data.table::setcolorder(
    age_rows,
    c(
      "method",
      "table",
      names(age_rows)[!names(age_rows) %in% c("method", "table")]
    )
  )

  ## Add age rows to table
  tab_for_gt <- data.table::rbindlist(
    list(
      age_rows,
      tab_for_gt[tab_for_gt$name != "Age"]
    ),
    use.names = TRUE
  )

  ## Make method and table factors so we can get correct order
  tab_for_gt[,
    c("method", "table") := list(
      factor(method, levels = c("LP Visits", "PET Visits")),
      factor(
        table,
        levels = c(
          "age",
          "Local Roche CSF - Sarstedt freeze 2, cleaned",
          "Local Roche CSF - Sarstedt freeze 3",
          "Local Roche CSF - Sarstedt freeze, cleaned",
          "NTK MultiObs - CSF analytes",
          "NTK2 MultiObs - CSF, 20230311",
          "HDX Plasma - pTau217",
          "MK6240_NFT_Rating",
          "NAV4694 Visual Ratings",
          "PIB Visual Rating 20180126"
        )
      )
    )
  ]

  data.table::setorder(
    tab_for_gt,
    method,
    table
  )

  ## After table has been ordered, convert back to character vectors to avoid weird factor behavior.
  tab_for_gt[,
    names(.SD) := lapply(.SD, as.character),
    .SDcols = c("method", "table")
  ]

  ## Add extra column at end of table for formatting
  tab_for_gt[,
    "append" := ""
  ]

  ## Function to create table header. We add two headers, hence the function.
  ## The argument is meant to give all columns that are actually valid visits
  ## for the method we're dealing with.
  table_header <- function(true_visits) {
    shiny::tags$tr(
      lapply(seq_along(tab_for_gt), \(x) {
        x_name <- colnames(tab_for_gt)[x]

        shiny::tags$th(
          ifelse(
            x_name %in%
              c("method", "table", "name", "append") |
              length(true_visits) == 0,
            "",
            x_name
          ),
          class = if (
            !x_name %in% c("method", "name", "table", "append", true_visits) &
              length(true_visits) > 0
          ) {
            prev_visit <- colnames(tab_for_gt)[x - 1] %in%
              c("method", "name", "table", "append", true_visits)

            next_visit <- colnames(tab_for_gt)[x + 1] %in%
              c("method", "name", "table", "append", true_visits)

            if (!prev_visit & !next_visit) {
              "lowered-column center"
            } else if (!prev_visit) {
              "lowered-column right"
            } else if (!next_visit) {
              "lowered-column left"
            } else {
              "lowered-column"
            }
          }
        )
      })
    )
  }

  ## tagList to hold actual table
  shiny::tagList(
    ## Add CSS
    shiny::includeCSS(path = paste(www_path, "biomarkerTable.css", sep = "/")),
    ## Add JS
    shiny::includeScript(
      path = paste(www_path, "biomarkerTable.js", sep = "/")
    ),
    ## Table-container
    shiny::tags$div(
      class = "table-container",
      ## Table
      shiny::tags$table(
        class = "biomarkerTable",
        ## Table body
        shiny::tags$tbody(
          lapply(1:nrow(tab_for_gt), \(x) {
            if (print_x) {
              print(x)
            }

            nam <- tab_for_gt$name[x]
            tab <- tab_for_gt$table[x]
            met <- tab_for_gt$method[x]

            ## Get vector of visits that are "true visits" for this method.
            ## This is based on valid age, and chosen among columns named
            ## by dates.
            true_visits <- tab_for_gt[
              method == met & name == "Age",
              lapply(.SD, \(x) {
                if (length(x) > 0 && is.na(unlist(x))) NULL else unlist(x)
              }),
              .SDcols = colnames(tab_for_gt)[
                grepl("20[0-9]{2}-[0-9]{2}-[0-9]{2}", colnames(tab_for_gt))
              ]
            ] |>
              colnames()

            ## Is this the last row of part of table related to this method?
            last_row <- (x == nrow(tab_for_gt)) ||
              (met != tab_for_gt$method[x + 1])

            ## If method is "new" (i.e. different from previous), we want to
            ## add header. If not, `subtable_header` will be NULL and ignored
            subtable_header <- if (!duplicated(tab_for_gt$method)[x]) {
              list(
                shiny::tags$tr(
                  class = "subtable-header",
                  shiny::tags$td(
                    colspan = ncol(tab_for_gt) - 1,
                    ## Add extra padding-top to second header
                    style = if (x > 1) "padding-top: 24px;",
                    shiny::HTML(paste("&nbsp;&nbsp;", met))
                  )
                ),
                table_header(true_visits)
              )
            }

            if (tab == nam) {
              cur_tr <- shiny::tags$tr(
                class = "row-group",
                shiny::tags$td(),
                shiny::tags$td(
                  tab,
                  colspan = ncol(tab_for_gt) - 2
                ),
                shiny::tags$td()
              )
            } else if (grepl("^Error|No values found", nam)) {
              cur_tr <- shiny::tags$tr(
                class = if (last_row) "last-row",
                shiny::tags$td(),
                shiny::tags$td(
                  nam,
                  class = ifelse(
                    grepl("^Error", nam),
                    "error-message",
                    "no-values"
                  ),
                  colspan = ncol(tab_for_gt) - 2
                ),
                shiny::tags$td()
              )
            } else {
              obs <- tab_for_gt[x, ]
              obs[, c("method", "table") := list("", "")]

              ## Is this the first row in a group of rows?
              first_row_in_group <- (nam == "Age") ||
                (tab != tab_for_gt$table[x - 2])

              ## Is this the last row in a group of rows?
              last_row_in_group <- (x == nrow(tab_for_gt)) ||
                (tab != tab_for_gt$table[x + 1])

              cur_tr <- shiny::tags$tr(
                class = paste(
                  c(
                    if (last_row) "last-row",
                    if (first_row_in_group) "first-row-in-group",
                    if (last_row_in_group) "last-row-in-group"
                  ),
                  collapse = " "
                ),
                # class = if (last_row) "last-row",
                unname(purrr::imap(
                  obs,
                  \(y, idy) {
                    # if (x == 3) {
                    #   browser()
                    # }

                    create_td(
                      y,
                      idy,
                      cell_id = paste(x, idy, sep = "_"),
                      is_age = nam == "Age",
                      class = NULL,
                      # class = paste(
                      #   c(
                      #     # if (last_row) "last-row",
                      #     if (first_row_in_group) "first-row-in-group",
                      #     if (last_row_in_group) "last-row-in-group"
                      #   ),
                      #   collapse = " "
                      # ),
                      true_visits,
                      prev_col = colnames(tab_for_gt)[
                        which(colnames(tab_for_gt) == idy) - 1
                      ],
                      next_col = colnames(tab_for_gt)[
                        which(colnames(tab_for_gt) == idy) + 1
                      ],
                      plot_title = list(
                        shiny::h5(nam, style = "font-weight: bold;"),
                        shiny::p(tab)
                      ),
                      # fmt: skip
                      cur_dens = if (tab %in% names(densities)) densities[[tab]][[paste(nam, "raw", sep = "_")]],
                      cur_cut = if (tab %in% names(cuts)) {
                        cuts[[tab]][name == nam, ]
                      }
                    )
                  }
                ))
              )
            }

            shiny::tagList(
              subtable_header,
              cur_tr
            )
          })
        )
      )
    )
  )
}


create_td <- function(
  y,
  idy,
  cell_id,
  is_age,
  class,
  true_visits,
  prev_col,
  next_col,
  plot_title,
  cur_dens = densities[[tab]][[paste(
    nam,
    "raw",
    sep = "_"
  )]],
  cur_cut = all_cuts[[tab]][name == nam, ]
) {
  # To avoid notes in R CMD check
  densities <- NULL
  all_cuts <- NULL
  tab <- NULL
  nam <- NULL
  name <- NULL

  ## Check if this is an LP/PET visit by checking if age is NA
  true_visit <- idy %in%
    c("method", "table", "name", "append", true_visits)

  if (!true_visit) {
    # fmt: skip
    prev_visit <- isTRUE(prev_col %in% c("method", "table", "name", "append", true_visits))

    # fmt: skip
    next_visit <- isTRUE(next_col %in% c("method", "table", "name", "append", true_visits))

    lowered_class <- if (!prev_visit & !next_visit) {
      "lowered-column center"
    } else if (!prev_visit) {
      "lowered-column right"
    } else if (!next_visit) {
      "lowered-column left"
    } else {
      "lowered-column"
    }

    return(
      shiny::tags$td(
        " ",
        class = paste(
          c(
            lowered_class,
            class
          ),
          collapse = " "
        )
      )
    )
  }

  if (idy %in% c("name", "table", "method", "append")) {
    return(
      shiny::tags$td(
        y,
        class = class,
        style = paste(
          "text-align: left;"
        )
      )
    )
  }

  if (is_age) {
    return(
      shiny::tags$td(
        y,
        class = class
      )
    )
  }

  if (is.null(unlist(y)) || is.na(y)) {
    return(
      shiny::tags$td(
        shiny::HTML("&mdash;"),
        class = class
        # class = paste(
        #   c(
        #     if (last_row) "last-row",
        #     if (first_row_in_group) "first-row-in-group",
        #     if (last_row_in_group) "last-row-in-group"
        #   ),
        #   collapse = " "
        # )
      )
    )
  }

  return(
    shiny::tags$td(
      class = paste(
        c(
          "flex-cell",
          class
        ),
        collapse = " "
      ),
      cell_content(
        cell = y[[1]],
        cell_id = cell_id,
        plot_title,
        cur_dens,
        cur_cut
      )
    )
  )
}


cell_content <- function(
  cell,
  cell_id,
  plot_title,
  dens,
  cur_cuts
) {
  shiny::tags$div(
    class = "flex-cell-wrapper",
    shiny::tags$span(
      class = "flex-cell-left",
      shiny::HTML(cell$icon)
    ),
    shiny::tags$span(
      class = "flex-cell-center",
      shiny::HTML(gsub(
        pattern = "/Indeterminate",
        replacement = "/&#8203;Indeterminate",
        x = cell$text
      ))
    ),
    if (!is.null(cell$raw)) {
      shiny::tags$span(
        class = "flex-cell-right plot-icon",
        shiny::icon("chart-line"),
        shiny::tags$div(
          id = paste0("hoverBox_", cell_id),
          class = "hover-box",
          if (!missingArg(plot_title)) plot_title,
          density_plot(
            obs = cell$raw,
            dens = dens,
            cuts = cur_cuts,
            height = 200,
            width = 400,
            new_id = paste("density-plotly", cell_id, sep = "-")
          )
        ),
        `data-tooltip` = shiny::HTML(sprintf(
          "Raw value: %.2f (Click icon for graph.)",
          cell$raw
        )),
        onclick = sprintf(
          "toggleHoverBox(event, '%s')",
          cell_id
        )
      )
    } else {
      shiny::tags$span(
        class = "flex-cell-right",
        style = "width: 24px;"
      )
    }
  )
}
