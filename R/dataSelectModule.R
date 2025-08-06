#' Shiny module to select data for assessment table
#'
#' @rdname dataSelectModule
#'
#' @description
#' A short description...
#'
#' @param id A string used to namespace the module.
#'
#' @returns
#' A UI definition.
#'
#' @export

dataSelectUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        open = "always",
        width = 400,
        bslib::card_body(
          gt::gt_output(ns("data_sources_table")),
          shiny::uiOutput(ns("submit_button"))
        )
      ),

      shiny::selectizeInput(
        inputId = ns("data_source"),
        label = "Data Source",
        choices = c(
          "Pull From REDCap" = "redcap",
          "Panda (biomarker)" = "panda",
          "Upload CSV File" = "csv_upload",
          "Upload Previously Saved Data Sources" = "retrieve",
          "Demo" = "demo"
        ),
        options = list(
          disabledField = "disabled",
          options = list(
            list(
              disabled = !rlang::is_installed("REDCapR")
            ),
            list(
              disabled = !rlang::is_installed("httr2") &&
                !rlang::is_installed("jsonlite")
            ),
            list(disabled = FALSE),
            list(disabled = FALSE),
            list(disabled = FALSE)
          )
        )
      ),
      shiny::uiOutput(ns("data_type_ui")),
      shiny::uiOutput(ns("data_upload_or_password")),
      shiny::uiOutput(ns("fetch_data"))
    ),
    height = "100vh"
  )
}

#' @rdname dataSelectModule
#'
#' @description
#' Server logic for dataSelectModule.
#'
#' @param id A string used to create a namespace within the shiny app.
#'
#' @returns
#' A list containing reactive values for the data object (`dat_obj`),
#'  the data source, the data type, and the biomarker API.
#'
#' @export
dataSelectServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ## UI for data type selection
    output$data_type_ui <- shiny::renderUI({
      missing_pkgs_messages <- shiny::tagList(
        if (!rlang::is_installed("REDCapR")) {
          shiny::markdown(
            "The package `REDCapR` is required to pull data from REDCap. Close the app and install the package using `install.pakages(\"REDCapR\")`."
          )
        },
        if (!rlang::is_installed("httr2") | !rlang::is_installed("jsonlite")) {
          missing_pkgs <- c("httr2", "jsonlite")[c(
            !rlang::is_installed("httr2"),
            !rlang::is_installed("jsonlite")
          )]

          pluralize <- if (length(missing_pkgs) > 1) "s" else NULL

          shiny::markdown(
            paste0(
              "The package",
              pluralize,
              " '",
              paste(missing_pkgs, collapse = "' and '"),
              "' ",
              c("is", "are")[as.numeric(!is.null(pluralize)) + 1],
              " needed to pull data from panda. ",
              "Close the app, install the package",
              pluralize,
              " using `install.packages(c('",
              paste0(missing_pkgs, collapse = "', '"),
              "'))`."
            )
          )
        }
      )

      if (input$data_source != "retrieve") {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = shiny::NS(id, "data_type"),
            label = "Data Type",
            choices = c(
              # "NACC" = "nacc",
              # "WLS" = "wls",
              "WADRC (UDS-2)" = "wadrc_uds2",
              "WADRC (UDS-3)" = "wadrc_uds3",
              "WADRC (UDS-4)" = "wadrc_uds4"
            ),
            options = list(
              placeholder = "Select an option below",
              onInitialize = I('function() { this.setValue(""); }')
            )
          ),
          missing_pkgs_messages
        )
      } else {
        shiny::tagList(
          shiny::fileInput(
            inputId = shiny::NS(id, "saved_data_file"),
            label = "Select File",
            accept = ".bin"
          ),
          shiny::textInput(
            inputId = shiny::NS(id, "data_file_key"),
            label = "Data File Password",
            value = NULL,
            placeholder = "Enter password to load data"
          )
        )
      }
    })

    shiny::observe({
      shiny::updateSelectizeInput(
        inputId = "data_type",
        choices = switch(
          input$data_source, # list(
          demo = c("NACC" = "nacc"),
          redcap = c(
            "WADRC (UDS-2)" = "wadrc_uds2",
            "WADRC (UDS-3)" = "wadrc_uds3",
            "WADRC (UDS-4)" = "wadrc_uds4"
            # "WLS" = "wls"
          ),
          csv_upload = c(
            "NACC" = "nacc",
            "WLS" = "wls",
            "WADRC (UDS-2)" = "wadrc_uds2",
            "WADRC (UDS-3)" = "wadrc_uds3",
            "WADRC (UDS-4)" = "wadrc_uds4"
          ),
          panda = c("Biomarker" = "biomarker")
        ),
        selected = switch(
          input$data_source,
          "demo" = "nacc",
          "panda" = "biomarker"
        ) # if (input$data_source == "demo") "nacc" else NULL
      )
    }) |>
      shiny::bindEvent(
        input$data_source
      )

    output$data_upload_or_password <- shiny::renderUI({
      shiny::req(input$data_source)
      shiny::req(input$data_type)

      out <- shiny::p(
        "Click the 'Go' button to continue with demo data."
      )

      if (input$data_source == "panda") {
        out <- shiny::tagAppendAttributes(
          shiny::passwordInput(
            inputId = shiny::NS(id, "panda_api_token"),
            label = "Panda API Token",
            value = NULL
          ),
          .cssSelector = "input",
          autocomplete = "current-password"
        )
      }

      if (input$data_source == "redcap") {
        uri_and_token <- NULL

        out <- shiny::tagList(
          shiny::tagAppendAttributes(
            shiny::textInput(
              inputId = shiny::NS(id, "redcap_uri"),
              label = "REDCap URL",
              value = NULL
            ),
            .cssSelector = "input",
            autocomplete = "username"
          ),
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "api_token"),
              label = "REDCap API Token",
              value = NULL
            ),
            .cssSelector = "input",
            autocomplete = "current-password"
          )
        )
      }

      if (input$data_source == "csv_upload") {
        out <- shiny::fileInput(
          inputId = shiny::NS(id, "input_file"),
          label = "Select File",
          accept = ".csv"
        )
      }

      if (input$data_source %in% c("redcap", "panda")) {
        out <- shiny::tagList(
          out,
          shiny::checkboxInput(
            inputId = shiny::NS(id, "show_password"),
            label = "Show token"
          )
        )
      }

      out
    })

    shiny::observe({
      session$sendCustomMessage(
        "showHidePassword",
        list(show = input$show_password)
      )
    }) |>
      shiny::bindEvent(input$show_password)

    output$fetch_data <- shiny::renderUI({
      redcap_uri_and_token <-
        !is.null(input$api_token) &&
        input$api_token != "" &&
        !is.null(input$redcap_uri) &&
        input$redcap_uri != ""

      redcap_data_ready <- (input$data_source == "redcap" &
        redcap_uri_and_token &
        isTRUE(input$data_type != ""))

      csv_file_ready <- input$data_source == "csv_upload" &
        !is.null(input$input_file) &
        isTRUE(input$data_type != "")

      panda_ready <- input$data_source == "panda" &
        !is.null(input$panda_api_token) &&
        input$panda_api_token != ""

      if (
        input$data_source == "demo" |
          redcap_data_ready |
          csv_file_ready |
          panda_ready
      ) {
        bslib::input_task_button(
          id = shiny::NS(id, "fetch_data_button"),
          label = "Add"
        )
      }

      if (
        input$data_source == "retrieve" &&
          !is.null(input$data_file_key) &&
          input$data_file_key != ""
      ) {
        bslib::input_task_button(
          id = shiny::NS(id, "retrieve_data_button"),
          label = "Upload"
        )
      }
    })

    shiny::observe({
      loaded_data_sources <- safer::retrieve_object(
        conn = input$saved_data_file$datapath,
        key = input$data_file_key
      )

      if (inherits(loaded_data_sources, "try-error")) {
        shiny::showNotification(
          type = "error",
          "Could not load data sources. Make sure the file is a valid .bin file."
        )
        return()
      }

      for (dat_source in loaded_data_sources) {
        i <- length(shiny::reactiveValuesToList(data_sources)) + 1

        data_sources[[as.character(i)]] <- dat_source
      }
    }) |>
      shiny::bindEvent(input$retrieve_data_button)

    data_sources <- shiny::reactiveValues()

    shiny::observe({
      ## Add data source to output reactiveValues object
      i <- length(shiny::reactiveValuesToList(data_sources)) + 1

      new_source <- list(
        data_source = input$data_source,
        data_type = input$data_type
      )

      if (input$data_source == "redcap") {
        new_source$redcap_auth <-
          list(
            redcap_uri = input$redcap_uri,
            token = input$api_token
          )
      }

      if (input$data_source == "panda") {
        req_panda <- try(
          expr = {
            httr2::request("https://panda.medicine.wisc.edu") |>
              httr2::req_perform()
          },
          TRUE
        )

        if (inherits(req_panda, "try-error")) {
          shiny::showNotification(
            type = "error",
            "Could not access https://panda.medicine.wisc.edu. Make sure you are connected to the campus network, either directly or via VPN."
          )
          new_source <- NULL
        } else {
          new_source$panda_auth <- input$panda_api_token
        }
      }

      if (input$data_source == "csv_upload") {
        new_source$data_file <- input$input_file$datapath
      }

      if (
        !is.null(new_source$redcap_auth$redcap_uri) &&
          new_source$redcap_auth$redcap_uri != ""
      ) {
        # Make sure URI ends with API\\/
        if (
          !grepl(pattern = "API\\/$", x = new_source$redcap_auth$redcap_uri)
        ) {
          # If that is not the case, check if it ends with API
          if (grepl(pattern = "API$", x = new_source$redcap_auth$redcap_uri)) {
            # If yes, add trailing /
            new_source$redcap_auth$redcap_uri <- paste0(
              new_source$redcap_auth$redcap_uri,
              "/"
            )
          } else {
            # If no, notify the user that the URI is invalid
            shiny::showNotification(
              type = "error",
              "The URI must end in \"API/\". Double check the URI."
            )

            new_source <- NULL
          }
        }
      }

      if (!is.null(new_source)) {
        data_sources[[as.character(i)]] <- new_source

        ## Update Data Type selectizeInput
        shiny::updateSelectizeInput(
          inputId = "data_type",
          choices = switch(
            input$data_source, # list(
            demo = c("NACC" = "nacc"),
            redcap = c(
              "WADRC (UDS-2)" = "wadrc_uds2",
              "WADRC (UDS-3)" = "wadrc_uds3",
              "WADRC (UDS-4)" = "wadrc_uds4"
              # "WLS" = "wls"
            ),
            csv_upload = c(
              "NACC" = "nacc",
              "WLS" = "wls",
              "WADRC (UDS-2)" = "wadrc_uds2",
              "WADRC (UDS-3)" = "wadrc_uds3",
              "WADRC (UDS-4)" = "wadrc_uds4"
            ),
            panda = c("Biomarker" = "biomarker")
          ),
          selected = switch(
            input$data_source,
            "demo" = "nacc",
            "panda" = "biomarker"
          ) # if (input$data_source == "demo") "nacc" else NULL
        )
      }
    }) |>
      shiny::bindEvent(
        input$fetch_data_button,
        ignoreInit = TRUE
      )

    output$data_sources_table <- gt::render_gt(
      {
        tmp <- shiny::reactiveValuesToList(data_sources)

        if (length(tmp) == 0) {
          for_out <- data.frame(
            .id = NA,
            data_source = NA,
            data_type = NA
          )
        } else {
          for_out <- lapply(tmp, \(x) {
            x[c("data_source", "data_type")]
          }) |>
            data.table::rbindlist(idcol = TRUE)
        }

        out <- gt::gt(for_out, id = "gt_data_sources") |>
          gt::sub_missing(missing_text = "") |>
          gt::cols_hide(".id") |>
          gt::cols_label(
            # ".id" = "",
            "data_source" = gt::md("*Source*"),
            "data_type" = gt::md("*Type*")
          ) |>
          gt::tab_header(
            title = gt::md("**Data Sources**")
          ) |>
          gt::tab_style(
            style = list(
              gt::cell_borders(sides = "top", style = "hidden"),
              gt::cell_text(align = "left")
            ),
            locations = list(
              gt::cells_title(),
              gt::cells_column_labels()
            )
          ) |>
          gt::tab_options(
            table.width = gt::pct(100)
          )

        if (length(tmp) == 0) {
          out <- out |>
            gt::tab_footnote(
              footnote = "Add data sources using the UI on the right"
            )
        }

        out
      }
    )

    output$save_data_sources <- shiny::downloadHandler(
      filename = function() {
        paste0("data_sources_", Sys.Date(), ".bin")
      },
      content = function(file) {
        safer::save_object(
          object = shiny::reactiveValuesToList(data_sources),
          key = input$data_file_key,
          conn = file
        )
      }
    )

    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Save Data Sources",
          easyClose = TRUE,
          footer = NULL,
          shiny::textInput(
            inputId = shiny::NS(id, "data_file_key"),
            label = "Enter a key to save data sources"
          ),
          shiny::downloadButton(
            outputId = shiny::NS(id, "save_data_sources"),
            label = "Save Data Sources"
          )
        )
      )
    }) |>
      shiny::bindEvent(input$save_data_sources)

    output$submit_button <- shiny::renderUI({
      if (length(shiny::reactiveValuesToList(data_sources)) == 0) {
        return()
      }

      shiny::tagList(
        shiny::p(
          "When all needed data sources have been added, click the 'Save Data Sources' below to save data sources for later use, or 'Load Data' button below to continue."
        ),
        shiny::actionButton(
          inputId = shiny::NS(id, "submit"),
          label = "Load Data"
        ),
        shiny::actionButton(
          inputId = shiny::NS(id, "save_data_sources"),
          label = "Save Data Sources"
        )
      )
    })

    dat_obj <- shiny::reactiveVal()
    data_source <- shiny::reactiveVal()
    data_type <- shiny::reactiveVal()
    biomarker_api <- shiny::reactiveVal()

    shiny::observe({
      all_data_sources <- shiny::reactiveValuesToList(data_sources)

      biomarker_sources <- all_data_sources[unlist(lapply(
        all_data_sources,
        \(x) !is.null(x$panda_auth)
      ))]

      other_sources <- all_data_sources[setdiff(
        names(all_data_sources),
        names(biomarker_sources)
      )]

      tmp <- lapply(
        other_sources,
        \(x) {
          cbind(
            uds = x$data_type,
            do.call(read_data, args = x)
          )
        }
      ) |>
        data.table::rbindlist(idcol = FALSE, fill = TRUE) |>
        fill_data_downup(
          ptid = "NACCID",
          visityr = "VISITYR",
          visitmo = "VISITMO",
          visitday = "VISITDAY",
          educ = "EDUC",
          constant_across_visits = c(
            "SEX",
            "RACE",
            "BIRTHYR",
            "BIRTHMO"
          )
        )

      uds <- NULL # for R check

      ## Set uds to NA if there is only one value for a visit
      tmp[,
        uds := ifelse(nrow(unique(.SD)) == 1, NA, uds),
        .SDcols = setdiff(colnames(tmp), "uds"),
        by = c("NACCID", "VISITYR", "VISITMO", "VISITDAY")
      ]

      ## For those visits with entries in both UDS-3 and UDS-4, keep only the UDS-4 data
      tmp <- tmp[
        is.na(uds) | uds == "wadrc_uds4"
      ] |>
        unique()

      ## Remove the uds column
      tmp$uds <- NULL

      ## Remove rows with missing VISITYR
      tmp <- tmp[!is.na(tmp$VISITYR)]

      tmp <- fill_data_downup(
        out = tmp,
        ptid = "NACCID",
        visityr = "VISITYR",
        visitmo = "VISITMO",
        visitday = "VISITDAY",
        educ = "EDUC",
        constant_across_visits = c("BIRTHYR", "BIRTHMO", "SEX", "RACE")
      )

      dat_obj(tmp)

      if ("csv" %in% all_data_sources$data_source) {
        data_source("csv")
      } else if (length(unique(all_data_sources$data_source)) == 1) {
        data_source(all_data_sources$data_source[1])
      } else if ("redcap" %in% all_data_sources$data_source) {
        data_source("redcap")
      }

      data_type("wadrc")

      if (length(biomarker_sources) > 0) {
        biomarker_api(biomarker_sources[[1]]$panda_auth)
      }

      shiny::updateTextInput(inputId = "redcap_uri", value = NULL)
      shiny::updateTextInput(inputId = "panda_api_token", value = NULL)
    }) |>
      shiny::bindEvent(input$submit)

    return(list(
      dat_obj = dat_obj,
      data_source = data_source,
      data_type = data_type,
      biomarker_api = biomarker_api
    ))
  })
}

#' @rdname dataSelectModule
#'
#' @param testing Logical, whether to run the app in testing mode.
#'
#' @export
dataSelectApp <- function(testing = FALSE) {
  shinyAddResources()

  ui <- bslib::page_fluid(
    # shiny::tagList(
    #   shiny::tags$head(
    #     shiny::tags$script(
    #       src = "www/scripts.js"
    #     ),
    #     shiny::tags$link(
    #       rel = "stylesheet",
    #       type = "text/css",
    #       href = "www/styles.css"
    #     )
    #   ),
    #   shiny::tags$div(id = "spinner", class = "loader"),
    #   shiny::tags$div(id = "spinner_overlay", class = "loader_overlay")
    # ),
    shinyApp_header(),
    dataSelectUI("dat_select"),
    shiny::actionButton("fetch_data", label = "Submit")
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 1000 * 1024^2)

    dat_obj <- shiny::reactiveVal()

    data_input <- dataSelectServer("dat_select")

    shiny::observe({
      dat_obj(data_input$dat_obj())

      shiny::showNotification(
        ui = paste(dim(dat_obj()), collapse = "; "),
        duration = 2
      )
    })
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
