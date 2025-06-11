#' Shiny Module to Plot Scores Across Visits
#'
#' This shiny module is meant for visualizing changes over time of a long list
#' of cognitive measures from the WLS ILIAD study.
#'
#' @param id An ID string to match module UI and module server
#' @param dat Data to use for the plots.
#' @param studyid ID of participant; reactive
#'
#' @inheritParams base_plot_z_scores
#'
#' @examples
#' \dontrun{
#' plotVarApp(demo_data[1, ])
#' }
#'
#' @rdname plotVarModule
#'
#' @export
plotVarUI <- function(id) {
  bslib::card_body(
    shiny::uiOutput(shiny::NS(id, "plotsUI")),
    fillable = F
  )
}

#' @rdname plotVarModule
#'
#' @param print_updating logical (defualt `FALSE`); should a message be displayed when table is being updated? For debugging.
#'
#' @export
#'

plotVarServer <- function(
  id,
  dat,
  studyid,
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
  print_updating = T,
  shade_descriptions = TRUE
) {
  # stopifnot("studyid must be a reactive" = shiny::is.reactive(studyid))

  if (!shiny::is.reactive(descriptions)) {
    descriptions <- shiny::reactiveVal(descriptions)
  }

  if (!shiny::is.reactive(fill_values)) {
    fill_values <- shiny::reactiveVal(fill_values)
  }

  if (!shiny::is.reactive(shade_descriptions)) {
    shade_descriptions <- shiny::reactiveVal(shade_descriptions)
  }

  shiny::moduleServer(id, function(input, output, session) {
    ## Create UI for plots
    output$plotsUI <- shiny::renderUI({
      ## Initiate list to hold plot UIs
      plot_output_list <- as.list(unique(nacc_var_groups))
      names(plot_output_list) <- paste(
        unique(nacc_var_groups),
        "plot",
        sep = "_"
      )

      ## For entry in list...
      plot_output_list <- lapply(plot_output_list, \(cur_group) {
        ## ... get vector of variables in group
        cur_cog_vars <- names(nacc_var_groups[nacc_var_groups == cur_group])

        tooltip_text <- FALSE

        if (cur_group == "General Cognition") {
          tooltip_text <- "When both MoCA and MMSE are available, these are plotted as one line, but with different markers. Hover markers to see details."
        }

        if (cur_group == "Attention/Processing") {
          tooltip_text <- shiny::HTML(
            "When both of the following pairs of scores are present, these are plotted as one line, but with different markers. Hover markers to see details.<br>
        - Digit Span Forward <> Number Span Forward.<br>
        - Digit Span Backward <> Number Span Backward."
          )
        }

        if (cur_group == "Memory") {
          tooltip_text <- shiny::HTML(
            "When both of the following pairs of scores are present, these are plotted as one line, but with different markers. Hover markers to see details.<br>
        - Logical Memory IA, Immediate <> Craft Immediate - Paraphrase.<br>
        - Logical Memory IIA, Delayed <> Craft Delay - Paraphrase."
          )
        }

        if (cur_group == "Language") {
          tooltip_text <- "When both Boston Naming Test and MINT are available, these are plotted as one line, but with different markers. Hover markers to see details."
        }

        if (!isFALSE(tooltip_text)) {
          cardtitle <- bslib::tooltip(
            shiny::h5(
              cur_group,
              .noWS = "outside"
            ) |>
              shiny::tagAppendAttributes(
                style = "text-decoration-line: underline; text-decoration-style: dotted;"
              ),
            tooltip_text,
            options = list(customClass = "my-tooltip")
          )
        } else {
          cardtitle <- shiny::h5(cur_group, .noWS = "outside")
        }

        ## Return card with a title and plotlyOutput
        bslib::card(
          cardtitle,
          bslib::card_body(
            plotly::plotlyOutput(
              shiny::NS(id, paste(cur_group, "plot", sep = "_")),
              width = "100%"
            )
          )
        )
      })

      ## Return all cards as tagList
      base::do.call(shiny::tagList, plot_output_list)
    })

    ## Reactive value used to trigger drawing of traces when base_plots are created
    base_plots_drawn <- shiny::reactiveVal(value = 0)

    ## When data input changes, create base plots.
    shiny::observe({
      if (is.data.frame(dat()) | inherits(dat(), "data.table")) {
        if (nrow(dat()) > 0) {
          if (print_updating) {
            print("Creating base plots...")
          }

          ## Create base plot for each group of variables
          for (var_group in unique(nacc_var_groups)) {
            local({
              my_var_group <- var_group
              ## Need to use local so that the loop variable is evaluated in the right
              ## namespace
              cur_vars <- paste(
                "std",
                names(nacc_var_groups[nacc_var_groups == my_var_group]),
                sep = "_"
              )

              tmp_dat <- dat()

              # Are there any non-missing values for this group of variables?
              tmp_dat <- tmp_dat[,
                lapply(.SD, \(x) if (any(!is.na(x))) x),
                .SDcols = intersect(cur_vars, colnames(tmp_dat))
              ]

              no_obs <- ncol(tmp_dat) == 0

              output[[paste(
                my_var_group,
                "plot",
                sep = "_"
              )]] <- plotly::renderPlotly({
                base_plot_z_scores(
                  dat(),
                  nacc_vars = gsub(
                    pattern = "^std_",
                    replacement = "",
                    x = colnames(tmp_dat)
                  ),
                  fill_values = fill_values(),
                  descriptions = descriptions(),
                  shade_descriptions = shade_descriptions(),
                  source = my_var_group
                ) |>
                  plotly::layout(
                    showlegend = TRUE,
                    hoverlabel = list(align = "left"),
                    legend = list(
                      traceorder = "normal"
                    ),
                    xaxis = list(
                      title = ""
                    )
                  ) |>
                  plotly::event_register(event = "plotly_legendclick") |>
                  plotly::event_register(event = "plotly_legenddoubleclick") |>
                  onRender(
                    "function(el, x, input) {
                      el.on('plotly_afterplot', function(eventdata) {
                        var out = [];
                        // Function to get needed info from traces.
                        function getTraceInfo(trace, traceindex) {
                          // If trace has a name, set tracename
                          if (typeof trace.name !== 'undefined') {
                            var tracename = trace.name ;
                          } else {
                            var tracename = '';
                          }

                          // If trace has visible attribute, set tracevisible
                          if (typeof trace.visible !== 'undefined') {
                            var tracevisible = trace.visible ;
                          } else {
                            var tracevisible = '';
                          }

                          // If trace has customdata attribute, set name and create
                          // input$ns-name_visibility
                          if (typeof trace.customdata !== 'undefined') {
                            var name = trace.customdata ;

                            Shiny.setInputValue(input.ns + '-' + name + '_visibility', tracevisible);
                          } else {
                            var name = '';
                          }

                          // Add to out list
                          out.push([tracename=tracename, index=traceindex, name = name, visible = tracevisible]);
                        }

                        // Run function for each trace
                        x.data.forEach(getTraceInfo);

                        // Create input$input.name
                        Shiny.setInputValue(input.name, out);

                        // Create input$base_plots_drawn to indicate base plots have been created.
                        Shiny.setInputValue(input.ns + '-base_plots_drawn', 1)
                      });
                    }",
                    data = list(
                      name = shiny::NS(
                        id,
                        paste(my_var_group, "TraceMapping", sep = "_")
                      ),
                      ns = id
                    )
                  )
              })
            })
          }
          base_plots_drawn(base_plots_drawn() + 1)
        }
      }
    }) |>
      shiny::bindEvent(
        dat(),
        ignoreNULL = TRUE # ,
        # ignoreInit = TRUE
      )

    visibility_defaults <- list(
      "CDRGLOB" = TRUE,
      "MOCATOTS" = TRUE,
      "MOCBTOTS" = TRUE,
      "NACCMMSE" = TRUE,
      # "cog_ticsm" = TRUE,
      "TRAILA" = TRUE,
      "OTRAILA" = TRUE,
      "OTRLARR" = "legendonly",
      "DIGFORCT" = "legendonly",
      "DIGIF" = "legendonly",
      "DIGFORSL" = TRUE,
      "DIGIFLEN" = TRUE,
      "DIGBACCT" = "legendonly",
      "DIGIB" = "legendonly",
      "DIGBACLS" = TRUE,
      "DIGIBLEN" = TRUE,
      "WAIS" = TRUE,
      "MINTTOTS" = TRUE,
      "BOSTON" = TRUE,
      "ANIMALS" = TRUE,
      "VEG" = TRUE,
      "UDSVERTN" = "legendonly",
      # "cog_flc_flu" = TRUE,
      "UDSVERFC" = TRUE,
      "UDSVERLC" = TRUE,
      "UDSBENTC" = TRUE,
      "UDSBENTD" = TRUE,
      "CRAFTVRS" = "legendonly",
      "CRAFTURS" = TRUE,
      "LOGIMEM" = TRUE,
      "CRAFTDVR" = "legendonly",
      "CRAFTDRE" = TRUE,
      "MEMUNITS" = TRUE,
      "REYTOTAL" = TRUE,
      "REYDLIST" = "legendonly",
      "REY6REC" = "legendonly",
      "REYDREC" = TRUE,
      "REYAREC" = TRUE,
      "TRAILB" = TRUE,
      "OTRAILB" = TRUE,
      "OTRLBRR" = TRUE,
      "MOCACLOCK" = TRUE,
      "NACCGDS" = TRUE
    )

    ## Toggle shades
    shiny::observe({
      ## For each group, update visible attribute for shades
      lapply(unique(nacc_var_groups), FUN = \(cur_var_group) {
        if (
          !is.null(input[[paste(cur_var_group, "TraceMapping", sep = "_")]])
        ) {
          traces <- matrix(
            input[[paste(cur_var_group, "TraceMapping", sep = "_")]],
            ncol = 4,
            byrow = TRUE
          )

          indices <- traces[, 2][
            traces[, 1] %in% names(descriptions())
          ]

          plotly::plotlyProxy(
            paste(cur_var_group, "plot", sep = "_"),
            session
          ) |>
            plotly::plotlyProxyInvoke(
              method = "restyle",
              list(
                visible = rep(list(shade_descriptions()), length(indices))
              ),
              indices
            )
        }
      })
    }) |>
      shiny::bindEvent(
        shade_descriptions()
      )

    cog_vars_colors <- unlist(lapply(unique(nacc_var_groups), \(x) {
      group_vars <- names(nacc_var_groups[nacc_var_groups == x])

      setNames(
        rep(
          c(
            "#A6CEE3",
            "#1F78B4",
            "#B2DF8A",
            "#33A02C",
            "#FB9A99",
            "#E31A1C",
            "#FDBF6F",
            "#FF7F00",
            "#CAB2D6",
            "#6A3D9A",
            "#B15928"
          ),
          length.out = length(group_vars)
        ),
        nm = group_vars
      )
    }))

    legend_names <- shiny::reactiveVal(nacc_var_labels)

    shiny::observe({
      shiny::req(studyid())

      if (base_plots_drawn() > 0 & studyid() %in% dat()$NACCID) {
        if (print_updating) {
          print("Updating plots...")
        }

        cur_studyid_dat <- dat()[dat()$NACCID == studyid(), ]

        lapply(unique(nacc_var_groups), \(cur_var_group) {
          cur_vars <- names(nacc_var_groups)[nacc_var_groups == cur_var_group]

          if (length(cur_vars) == 0) {
            return()
          }

          # Remove old trace if any
          if (
            !is.null(input[[paste(cur_var_group, "TraceMapping", sep = "_")]])
          ) {
            traces <- matrix(
              input[[paste(cur_var_group, "TraceMapping", sep = "_")]],
              ncol = 4,
              byrow = TRUE
            )
            indices <- as.integer(traces[
              traces[, 1] %in% c(legend_names(), "no_values"),
              2
            ])

            plotly::plotlyProxy(
              paste(cur_var_group, "plot", sep = "_"),
              session
            ) |>
              plotly::plotlyProxyInvoke("deleteTraces", indices)

            if (print_updating) print("Old traces removed...")
          } else {
            if (print_updating) print("No old traces to remove...")
          }

          ## Select only VISITDATE, raw, and std columns, and only if there is at least one non-missing value
          new_dat <- cur_studyid_dat[,
            lapply(.SD, \(x) if (any(!is.na(x))) x),
            .SDcols = c(
              "VISITDATE",
              intersect(
                c(
                  paste("std", cur_vars, sep = "_"),
                  paste("raw", cur_vars, sep = "_")
                ),
                colnames(cur_studyid_dat)
              )
            )
          ]

          ## Adjust T-scores
          t_score_cols <- names(which(unlist(lapply(
            new_dat,
            \(x) attributes(x)$method == "T-score"
          ))))

          for (col in t_score_cols) {
            new_dat[[col]] <- (new_dat[[col]] - 50) / 10
          }

          ## Check if any standardized values
          any_left <- sum(grepl(pattern = "^std_", x = colnames(new_dat)))

          if (is.null(any_left)) {
            return()
          }

          if (any_left > 0) {
            ## Due to NSE notes in R CMD check:
            name <- value.name <- NULL

            new_dat <- data.table::melt(
              new_dat,
              id.vars = "VISITDATE",
              measure.vars = data.table::measure(
                ## special keyword value.name is similar to .value in pivot_longer
                value.name,
                name,
                pattern = "(raw|std)_(.*)"
              )
            )

            new_dat$label <- unname(nacc_var_labels[new_dat$name])

            crosswalk_translations <- c(
              "NACCMMSE" = "MOCATOTS",
              "DIGIF" = "DIGFORCT",
              "DIGIFLEN" = "DIGFORSL",
              "DIGIB" = "DIGBACCT",
              "DIGIBLEN" = "DIGBACLS",
              "BOSTON" = "MINTTOTS",
              "LOGIMEM" = "CRAFTURS",
              "MEMUNITS" = "CRAFTDRE"
            )

            new_dat$name[new_dat$name %in% names(crosswalk_translations)] <-
              crosswalk_translations[new_dat$name[
                new_dat$name %in% names(crosswalk_translations)
              ]]

            new_dat <- new_dat[!is.na(new_dat$std), ]

            lapply(unique(new_dat$name), \(nm) {
              tmp_dat <- new_dat[new_dat$name == nm, ]
              tmp_dat <- tmp_dat[order(tmp_dat$VISITDATE)]

              if (paste(nm, "visibility", sep = "_") %in% names(input)) {
                vis <- input[[paste(nm, "visibility", sep = "_")]]
              } else {
                vis <- visibility_defaults[[nm]]
              }

              ## If multiple labels, i.e. if dealing with crosswalk scores,
              ## combine to one label.
              if (length(unique(tmp_dat$label)) > 1) {
                legend_name <- paste(unique(tmp_dat$label), collapse = "/")

                if (
                  grepl(
                    pattern = "\\ \\-\\ Total|\\ \\- Span\\ Length",
                    x = legend_name
                  )
                ) {
                  if (grepl(pattern = "\\ \\-\\ Total", x = legend_name)) {
                    legend_name <- paste0(
                      gsub(
                        pattern = "\\ \\-\\ Total",
                        replacement = "",
                        x = legend_name
                      ),
                      " - Total"
                    )
                  }

                  if (
                    grepl(pattern = "\\ \\-\\ Span Length", x = legend_name)
                  ) {
                    legend_name <- paste0(
                      gsub(
                        pattern = "\\ \\-\\ Span\\ Length",
                        replacement = "",
                        x = legend_name
                      ),
                      " - Span Length"
                    )
                  }
                }
              } else {
                legend_name <- unique(tmp_dat$label)
              }

              legend_names(
                unique(c(legend_names(), legend_name))
              )

              new_lines <- list(
                type = "scatter",
                mode = "lines",
                x = as.list(tmp_dat$VISITDATE),
                y = as.list(tmp_dat$std),
                text = I(tmp_dat$label),
                line = list(
                  color = cog_vars_colors[[nm]],
                  line = list(color = cog_vars_colors[[nm]])
                ),
                name = legend_name,
                visible = vis,
                legendgroup = legend_name,
                customdata = nm,
                hovertemplate = paste0(
                  '<span style="font-weight:bold;">%{text}</span><br>',
                  "Visit Date: %{x}<br>",
                  "Scores:<br>",
                  " - raw: ",
                  tmp_dat$raw,
                  "<br>",
                  " - std: %{y:.2f}",
                  "<extra><br>",
                  "</extra>"
                )
              )

              if (length(unique(tmp_dat$label)) > 1) {
                symbs <- as.list(c("diamond", "cross")[as.numeric(factor(
                  tmp_dat$label
                ))])
              } else {
                symbs <- "o"
              }

              new_markers <- list(
                type = "scatter",
                mode = "markers",
                x = as.list(tmp_dat$VISITDATE),
                y = as.list(tmp_dat$std),
                text = I(tmp_dat$label),
                marker = list(
                  color = cog_vars_colors[[nm]],
                  symbol = symbs,
                  size = 8
                ),
                name = legend_name,
                visible = vis,
                showlegend = F,
                legendgroup = legend_name,
                customdata = nm,
                hovertemplate = paste0(
                  '<span style="font-weight:bold">%{text}</span><br>',
                  "Visit Date: %{x}<br>",
                  "Scores:<br>",
                  " - raw: ",
                  tmp_dat$raw,
                  "<br>",
                  " - std: %{y:.2f}",
                  "<extra><br>",
                  "</extra>"
                )
              )

              plotly::plotlyProxy(
                paste(cur_var_group, "plot", sep = "_"),
                session
              ) |>
                plotly::plotlyProxyInvoke(
                  method = "addTraces",
                  new_lines
                ) |>
                plotly::plotlyProxyInvoke(
                  method = "addTraces",
                  new_markers
                )
            })

            plotly::plotlyProxy(
              paste(cur_var_group, "plot", sep = "_"),
              session
            ) |>
              plotly::plotlyProxyInvoke(
                method = "reconfig",
                staticPlot = FALSE
              ) |>
              plotly::plotlyProxyInvoke(
                method = "relayout",
                list(
                  xaxis = list(
                    automargin = TRUE,
                    range = date_range(new_dat$VISITDATE)
                  ),
                  yaxis = list(
                    title = "z-score",
                    y = as.list(0),
                    yanchor = "bottom",
                    yref = "container",
                    range = c(
                      min(-2.5, min(new_dat$std, na.rm = T)),
                      max(2.5, max(new_dat$std, na.rm = T))
                    ) *
                      1.02
                  ),
                  showlegend = TRUE
                )
              )
          } else {
            # If no values found

            plotly::plotlyProxy(
              paste(cur_var_group, "plot", sep = "_"),
              session
            ) |>
              plotly::plotlyProxyInvoke(
                method = "relayout",
                list(
                  xaxis = list(range = c(-0.5, 0.5), visible = F),
                  yaxis = list(range = c(-0.5, 0.5), visible = F)
                )
              ) |>
              plotly::plotlyProxyInvoke(
                method = "addTraces",
                list(
                  x = list(0),
                  y = list(0),
                  textfont = list(size = 20),
                  text = list("No standardized values found for this group"),
                  type = "scatter",
                  mode = "text",
                  name = "no_values",
                  showlegend = FALSE
                )
              ) |>
              plotly::plotlyProxyInvoke(
                method = "reconfig",
                staticPlot = TRUE
              )
          }
        })
      }
    }) |>
      shiny::bindEvent(
        studyid(),
        dat(),
        input$base_plots_drawn,
        base_plots_drawn(),
        ignoreInit = T
      )

    shiny::observe({
      shiny::req(input[["General Cognition_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "General Cognition"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Attention/Processing_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Attention/Processing"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Language_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Language"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Visuospatial_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Visuospatial"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Memory_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Memory"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Executive Functioning_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Executive Functioning"
          )$x
        )
      )
    })

    shiny::observe({
      shiny::req(input[["Mood_TraceMapping"]])

      session$sendCustomMessage(
        "setInputValue",
        message = list(
          inputId = "update_date",
          inputValue = plotly::event_data(
            "plotly_click",
            source = "Mood"
          )$x
        )
      )
    })
  })
}


# 322002g
# 115163g

#' Shiny App Wrapping the variable plotting module
#'
#' @param studyids vector of studyids to include in drop-down menu
#'
#' @rdname plotVarModule
#'
#' @export
plotVarApp <- function(
  dat = prepare_data(demo_data),
  studyids = NULL # c("143050g", "241123g", "301022s", "322002g", "372034g", "382030s", "391310s", "440250g", "461231g", "498013s", "524005g", "595004s", "635012g")
) {
  ui <- bslib::page_sidebar(
    theme = bslib::bs_theme(
      version = 5
    ) |>
      bslib::bs_add_rules(
        ".my-tooltip .tooltip-inner {
          min-width: 500px;
          text-align: left;
        }"
      ),
    sidebar = bslib::sidebar(
      shiny::selectizeInput(
        inputId = "studyid",
        label = "Study ID",
        choices = NULL,
        selected = NULL
      ) # ,
      # shiny::checkboxInput(
      #   inputId = "brighter_colors",
      #   label = "Use Brighter Colors"
      # )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        id = "main-table",
        full_screen = T,
        bslib::card_header(
          "NACC T-Cog Neuropsychological Assessment Summary Table"
        ),
        bslib::card_body(
          shiny::selectInput(
            inputId = "test_date",
            label = "Date",
            choices = NULL
          ),
          mainTableUI("main_table")
          # verbatimTextOutput(NS("plot_cog_var", "click"))
          # verbatimTextOutput(NS("plot_cog_var", "PrintTraceMapping"))
        )
      ),
      bslib::card(
        id = "main-plot",
        full_screen = T,
        bslib::card_header("Longitudinal Trends"),
        plotVarUI("plot_cog_var")
      )
    )
  )

  server <- function(input, output, session) {
    ## Color scales
    # color_scales <- list(
    #   setNames(
    #     RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
    #     nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    #   ),
    #   setNames(
    #     scales::div_gradient_pal(low = "red", mid = "yellow", high = "green")(0:6/6),
    #     nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    #   )
    # )

    if (is.null(studyids)) {
      studyids <- unique(dat$NACCID)
    }

    shiny::updateSelectizeInput(
      inputId = "studyid",
      choices = studyids,
      server = TRUE
    )
    shiny::observeEvent(input$studyid, {
      shiny::updateSelectInput(
        inputId = "test_date",
        choices = dat$VISITDATE[dat$NACCID == input$studyid]
      )
    })

    plotVarServer(
      "plot_cog_var",
      dat = shiny::reactive(dat),
      studyid = shiny::reactive(input$studyid)
    )

    mainTableServer(
      "main_table",
      dat = shiny::reactive(
        dat[
          dat$NACCID == input$studyid & dat$VISITDATE == input$test_date
        ]
      ),
      methods = "infer",
      table_font_size = 80
    )
  }

  shiny::shinyApp(ui, server)
}
