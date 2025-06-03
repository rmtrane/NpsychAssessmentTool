#' Server Logic for shinyAssessmentApp
#'
#' @keywords internal
#'
#' @export
appServer <- function(input, output, session) {
  ## Hide 'Participant Data' on startup
  bslib::nav_hide(id = "main_navbar", target = "colSelect")
  bslib::nav_hide(id = "main_navbar", target = "tables-and-figures")

  ## Setup data select module
  dat_sel <- dataSelectServer("dataSelect")

  ## Reactive values to store data object, selected data source, and data type,
  ## all assigned from dataSelect module. Also, reactive value to indicate if
  ## user should be allowed to select columns for variables. We only allow this
  ## for csv upload.
  dat_obj <- shiny::reactiveVal()
  data_source <- shiny::reactiveVal()
  data_type <- shiny::reactiveVal()
  allow_col_selections <- shiny::reactiveVal()

  shiny::observe({
    dat_obj(dat_sel$dat_obj())
    data_source(dat_sel$data_source())
    data_type(dat_sel$data_type())
    allow_col_selections(data_source() == "csv_upload")
  })

  ## Reactive object with available columns to use to select from
  cols_avail <- shiny::reactive({
    colnames(dat_obj())
  })

  ## Reactive values to hold selected columns, and methods
  col_sel <- shiny::reactiveVal()
  std_methods <- shiny::reactiveVal()

  ## When dat_obj changes, flush selected columns and methods
  shiny::observe({
    col_sel(NA)
    std_methods(NA)
  }) |>
    shiny::bindEvent(
      dat_obj()
    )

  ## Select columns
  colSelectOutput <- colSelectServer(
    "colSelect",
    col_names = cols_avail,
    data_type = data_type,
    default_methods = list(
      MOCATOTS = c(method = "regression", version = "nacc"),
      OTRAILA = c(method = "regression", version = "updated"),
      OTRAILB = c(method = "regression", version = "updated"),
      # OTRLARR = c(method = "regression", version = "updated"),
      # OTRLBRR = c(method = "regression", version = "updated"),
      DIGFORCT = c(method = "regression", version = "nacc"),
      DIGFORSL = c(method = "regression", version = "nacc"),
      DIGBACCT = c(method = "regression", version = "nacc"),
      DIGBACLS = c(method = "regression", version = "nacc"),
      TRAILA = c(method = "regression", version = "nacc"),
      TRAILB = c(method = "regression", version = "nacc"),
      WAIS = c(method = "T-score", version = NA),
      MINTTOTS = c(method = "regression", version = "nacc"),
      ANIMALS = c(method = "regression", version = "nacc"),
      VEG = c(method = "regression", version = "nacc"),
      UDSVERFC = c(method = "regression", version = "nacc"),
      UDSVERLC = c(method = "regression", version = "nacc"),
      UDSVERTN = c(method = "regression", version = "nacc"),
      UDSBENTC = c(method = "regression", version = "nacc"),
      UDSBENTD = c(method = "regression", version = "nacc"),
      CRAFTVRS = c(method = "regression", version = "nacc"),
      CRAFTURS = c(method = "regression", version = "nacc"),
      CRAFTDVR = c(method = "regression", version = "nacc"),
      CRAFTDRE = c(method = "regression", version = "nacc"),
      # REY1REC = c(method = "T-score", version = NA),
      # REY2REC = c(method = "T-score", version = NA),
      # REY3REC = c(method = "T-score", version = NA),
      # REY4REC = c(method = "T-score", version = NA),
      # REY5REC = c(method = "T-score", version = NA),
      REY6REC = c(method = "T-score", version = NA),
      REYDREC = c(method = "T-score", version = NA),
      REYTOTAL = c(method = "T-score", version = NA),
      REYAREC = c(method = "T-score", version = NA),
      REYDLIST = c(method = "T-score", version = NA),
      NACCMMSE = c(method = "regression", version = "nacc"),
      BOSTON = c(method = "regression", version = "nacc"),
      LOGIMEM = c(method = "regression", version = "nacc"),
      MEMUNITS = c(method = "regression", version = "nacc"),
      DIGIF = c(method = "regression", version = "nacc"),
      DIGIFLEN = c(method = "regression", version = "nacc"),
      DIGIB = c(method = "regression", version = "nacc"),
      DIGIBLEN = c(method = "regression", version = "nacc")
    ),
    allow_col_selection = allow_col_selections()
  )

  shiny::observe({
    col_sel(colSelectOutput$var_cols())
    std_methods(colSelectOutput$std_methods())
  })

  shiny::observe({
    bslib::nav_show(id = "main_navbar", target = "colSelect", select = T)
  }) |>
    shiny::bindEvent(
      cols_avail(),
      ignoreInit = T
    )

  shiny::observe({
    bslib::nav_select(id = "main_navbar", selected = "colSelect")
  }) |>
    shiny::bindEvent(
      input$goToColSelect,
      ignoreInit = T,
      ignoreNULL = T
    )

  ## Prepare data
  fin_dat <- shiny::reactiveVal()

  shiny::observe({
    if (!all(is.na(std_methods())) & !all(is.na(col_sel()))) {
      fin_dat(
        #data.frame(
        prepare_data(
          dat_obj(),
          selected_cols = col_sel(),
          methods = std_methods(),
          print_messages = F
        )
        #)
      )
    }
  }) |>
    shiny::bindEvent(
      col_sel(),
      std_methods(),
      ignoreNULL = T,
      ignoreInit = T
    )

  shiny::observe({
    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    shiny::showModal(
      shiny::modalDialog(
        title = "Columns Recognized",
        easy_close = TRUE,
        "Enough NACC columns were recognized automatically. To make
                custom selections, go to the 'Setup' tab.",
        footer = bslib::layout_columns(
          cols_widths = c(4, -4, 4),
          shiny::actionButton("goToColSelect", label = "Setup"),
          shiny::modalButton("Dismiss")
        )
      )
    )
  }) |>
    shiny::bindEvent(input$moveToTables)

  shiny::observe({
    shiny::removeModal()
    bslib::nav_select(id = "main_navbar", selected = "colSelect")
  }) |>
    shiny::bindEvent(input$goToColSelect)

  ## Once data has been readied the first time, move to 'Participant Data'
  ## and update options for study ID dropdown.
  study_id_choices <- shiny::reactiveVal()

  shiny::observe({
    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    new_choices <- unique(fin_dat()$NACCID)

    if (
      is.null(study_id_choices()) |
        any(!new_choices %in% study_id_choices()) |
        (!is.null(devmode()) &&
          (devmode() & is.null(names(study_id_choices()))))
    ) {
      study_id_choices(sort(unique(fin_dat()$NACCID)))

      cur_choices <- study_id_choices()

      if (!is.null(devmode()) && devmode()) {
        n_visits <- table(fin_dat()$NACCID)[cur_choices]
        names(cur_choices) <- paste0(names(n_visits), " (", n_visits, ")")
      }

      if (
        !is.null(input$current_studyid) & input$current_studyid %in% cur_choices
      ) {
        cur_select <- input$current_studyid
      } else {
        cur_select <- cur_choices[1]
      }

      shiny::updateSelectizeInput(
        session,
        "current_studyid",
        choices = cur_choices,
        selected = cur_select, #cur_choices[1],
        server = TRUE
      )
    }
  }) |>
    shiny::bindEvent(
      fin_dat(),
      devmode(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

  ## Create demographics table
  output$demographics_table_output <- gt::render_gt({
    shiny::req(input$current_studyid)

    if (input$current_studyid %in% fin_dat()$NACCID) {
      demographics_table(
        subset(fin_dat(), fin_dat()$NACCID == input$current_studyid)
      )
    }
  })

  ## Update dropdown menu with visit dates when new study ID selected
  shiny::observe({
    dates <- fin_dat()$VISITDATE[fin_dat()$NACCID == input$current_studyid]

    sel_date <- NULL

    if (!is.null(selected_date()) && selected_date() %in% dates)
      sel_date <- selected_date()

    shiny::updateSelectizeInput(
      session,
      inputId = "current_date",
      choices = as.character(sort(unique(dates), decreasing = T)),
      selected = sel_date
    )

    selected_date(NULL)
  }) |>
    shiny::bindEvent(
      input$current_studyid,
      ignoreNULL = T,
      ignoreInit = T
    )

  ## When marker on one of the figures is clicked, input$update_date is set using session$sendCustomMessage (see plotVarModule.R)
  ## Here, we react to this event by setting the current date
  shiny::observe({
    shiny::req(input$update_date)

    shiny::updateSelectizeInput(
      session,
      inputId = "current_date",
      selected = input$update_date
    )
  })

  ## Setup reactiveVal for descriptions with default values
  descriptions <- shiny::reactiveVal(
    value = c(
      "Impaired" = 0.03,
      "Borderline" = 0.10,
      "Low Average" = 0.26,
      "Average" = 0.76,
      "High Average" = 0.92,
      "Superior" = 0.97,
      "Very Superior" = 1
    )
  )

  ## Setup reactiveVal for fill_values with default values
  fill_values <- shiny::reactiveVal(
    value = setNames(
      calc_fill_colors(7),
      c(
        "Impaired",
        "Borderline",
        "Low Average",
        "Average",
        "High Average",
        "Superior",
        "Very Superior"
      )
    )
  )

  ## Setup reactiveVal for devmode
  devmode <- shiny::reactiveVal(value = FALSE)

  ## Setup reactiveVal for table_font_size
  table_font_size <- shiny::reactiveVal(
    value = 80
  )

  ## Setup reactiveVal for shading
  shade_descriptions <- shiny::reactiveVal(
    value = TRUE
  )

  ## Server logic to let user modify description values and fill values.
  descriptions_and_fills <- descriptionsServer(
    id = "desc",
    default_descriptions = c(
      "Impaired" = 0.03,
      "Borderline" = 0.10,
      "Low Average" = 0.26,
      "Average" = 0.76,
      "High Average" = 0.92,
      "Superior" = 0.97,
      "Very Superior" = 1
    )
  )

  ## Subset full data to the data needed for the main assessment table
  dat_for_table <- shiny::reactive({
    shiny::req(input$current_date)

    # Note: use data.table since `[[` doesn't preserve attributes, which we need
    # to infer std. methods. Can be replaced by using data.table.
    fin_dat()[
      fin_dat()$NACCID == input$current_studyid &
        fin_dat()$VISITDATE == input$current_date
    ]
  })

  mainTableServer(
    "main_table",
    dat = dat_for_table,
    table_font_size = table_font_size,
    descriptions = descriptions,
    fill_values = fill_values,
    methods = std_methods,
    include_caption = T,
    print_updating = F
  )

  #### Longitudinal Trends

  ### Cognitive scores (Plots)
  plotVarServer(
    "plot_var",
    dat = fin_dat,
    studyid = shiny::reactive(input$current_studyid),
    descriptions = descriptions,
    fill_values = fill_values,
    print_updating = F,
    shade_descriptions = shade_descriptions
  )

  ### Cognitive scores (Table)
  ## Subset full data to the data needed for longitudinal and prev diagnoses tables
  dat_for_long <- shiny::reactive({
    shiny::req(input$current_studyid)

    # Note: use data.table since `[[` doesn't preserve attributes, which we need
    # to infer std. methods. Can be replaced by using data.table.
    fin_dat()[
      fin_dat()$NACCID == input$current_studyid
    ]
  })

  longTableServer(
    "long_table",
    dat = dat_for_long,
    methods = std_methods,
    table_font_size = table_font_size, # shiny::reactive(input$main_table_pct),
    fill_values = fill_values,
    descriptions = descriptions,
    print_updating = F
  )

  ## Diagnoses
  prevDiagnosesServer(
    "prev_diagnoses_table",
    dat = dat_for_long,
    table_font_size = table_font_size, # shiny::reactive(input$main_table_pct),
    print_updating = F
  )

  ## Update reactiveVals for values chosen in Options pane.
  selected_date <- shiny::reactiveVal()

  shiny::observe({
    bslib::accordion_panel_close(id = "options", values = TRUE)

    descriptions(
      descriptions_and_fills$descriptions()
    )

    fill_values(
      descriptions_and_fills$fill_values()
    )

    devmode(
      input$devmode
    )

    shade_descriptions(
      input$shade_descriptions
    )

    table_font_size(
      input$main_table_pct
    )

    ## Trigger a rerender of tables and plots by "poking" the input$current_studyid.
    ## First, get current date. This is used in the "update date".
    selected_date(input$current_date)

    session$sendCustomMessage(
      "setInputValue",
      list(
        inputId = "current_studyid",
        inputValue = input$current_studyid,
        priority = "event"
      )
    )
  }) |>
    shiny::bindEvent(
      input$update_colors
    )
}
