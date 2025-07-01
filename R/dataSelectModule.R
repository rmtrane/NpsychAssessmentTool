#' Shiny module to select data for assessment table
#'
#' @rdname dataSelectModule
#'
#' @param id id to link shiny modules
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

      # class = "align-items-center",
      shiny::selectizeInput(
        inputId = ns("data_source"),
        label = "Data Source",
        choices = c(
          "Pull From REDCap" = "redcap",
          "Upload CSV File" = "csv_upload",
          "Demo" = "demo"
        ) # ,
        # width = "300px"
      ),
      shiny::uiOutput(ns("data_type_ui")),
      shiny::uiOutput(ns("data_upload_or_password")),
      shiny::uiOutput(ns("fetch_data"))
    ),
    height = "100vh"
    #min_height = "400px"
  )
}

#' @rdname dataSelectModule
#'
#' @export
dataSelectServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$data_type_ui <- shiny::renderUI({
      if (
        input$data_source == "redcap" &
          (!rlang::is_installed("REDCapR"))
      ) {
        # session$sendCustomMessage(
        #   "setInputValue",
        #   message = list(inputId = shiny::NS(id, "data_type"), value = "")
        # )
        # session$sendCustomMessage(
        #   "setInputValue",
        #   message = list(inputId = shiny::NS(id, "input_file"), value = "")
        # )
        shiny::div(
          style = "width: 300px; color: red; font-weight: bold;",
          shiny::markdown(
            "The package `REDCapR` is required to pull data from REDCap. Close the app, install the package using `install.pakages(\"REDCapR\")`, and try again."
          )
        )
      } else {
        shiny::selectizeInput(
          inputId = shiny::NS(id, "data_type"),
          label = "Data Type",
          choices = c(
            # "NACC" = "nacc",
            # "WLS" = "wls",
            "WADRC (UDS-3)" = "wadrc_uds3",
            "WADRC (UDS-4)" = "wadrc_uds4"
          ),
          options = list(
            placeholder = "Select an option below",
            onInitialize = I('function() { this.setValue(""); }')
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
            "WADRC (UDS-3)" = "wadrc_uds3",
            "WADRC (UDS-4)" = "wadrc_uds4"
            # "WLS" = "wls"
          ),
          csv_upload = c(
            "NACC" = "nacc",
            "WLS" = "wls",
            "WADRC (UDS-3)" = "wadrc_uds3",
            "WADRC (UDS-4)" = "wadrc_uds4"
          )
        ), #[[input$data_source]],
        selected = if (input$data_source == "demo") "nacc" else NULL
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

      if (input$data_source == "redcap") {
        uri_and_token <- NULL

        # if (input$data_type == "wadrc_uds3") {
        #   uri_and_token <- getOption("redcap_adrc_uds3")
        # }

        # if (input$data_type == "wls") {
        #   uri_and_token <- getOption("redcap_wls")
        # }

        if (!is.null(uri_and_token)) {
          shiny::showNotification(
            "Locally stored REDCap URL and API token found and loaded.",
            duration = 5,
            type = "message",
            id = "redcap_url_api_found"
          )
        }

        out <- shiny::tagList(
          shiny::tagAppendAttributes(
            shiny::textInput(
              inputId = shiny::NS(id, "redcap_uri"),
              label = "REDCap URL",
              value = NULL #uri_and_token$redcap_uri
            ),
            .cssSelector = "input",
            autocomplete = "username"
          ),
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "api_token"),
              label = "REDCap API Token",
              value = NULL #uri_and_token$token
            ),
            .cssSelector = "input",
            autocomplete = "current-password"
          )
        )
        # }
      }

      if (input$data_source == "csv_upload") {
        out <- shiny::fileInput(
          inputId = shiny::NS(id, "input_file"),
          label = "Select File",
          accept = ".csv"
        )
      }

      out
    })

    output$fetch_data <- shiny::renderUI({
      uri_and_token <- shiny::reactive(
        !is.null(input$api_token) &&
          input$api_token != "" &&
          !is.null(input$redcap_uri) &&
          input$redcap_uri != ""
      )

      if (
        input$data_source == "demo" |
          (input$data_source == "redcap" &
            uri_and_token() &
            isTRUE(input$data_type != "")) |
          (input$data_source == "csv_upload" &
            !is.null(input$input_file) &
            isTRUE(input$data_type != ""))
      ) {
        bslib::input_task_button(
          id = shiny::NS(id, "fetch_data_button"),
          label = "Add"
        )
      }
    })

    data_sources <- shiny::reactiveValues()

    shiny::observe({
      ## Add data source to output reactiveValues object
      i <- length(shiny::reactiveValuesToList(data_sources)) + 1

      new_source <- list(
        # dat_obj = dat_obj,
        data_source = input$data_source,
        data_type = input$data_type,
        redcap_auth = list(
          redcap_uri = input$redcap_uri,
          token = input$api_token
        ),
        data_file = input$input_file$datapath
      )

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
              "WADRC (UDS-3)" = "wadrc_uds3",
              "WADRC (UDS-4)" = "wadrc_uds4"
              # "WLS" = "wls"
            ),
            csv_upload = c(
              "NACC" = "nacc",
              "WLS" = "wls",
              "WADRC (UDS-3)" = "wadrc_uds3",
              "WADRC (UDS-4)" = "wadrc_uds4"
            )
          ), #[[input$data_source]],
          selected = if (input$data_source == "demo") "nacc" else NULL
        )
      }
    }) |>
      shiny::bindEvent(
        input$fetch_data_button,
        ignoreInit = TRUE
      )

    output$data_sources_table <- gt::render_gt(
      {
        tmp <- reactiveValuesToList(data_sources)

        if (length(tmp) == 0) {
          for_out <- data.frame(
            .id = NA,
            data_source = NA,
            data_type = NA
          )
        } else {
          for_out <- lapply(tmp, \(x) {
            x$redcap_auth <- NULL
            x$data_file <- NULL

            x
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

    output$submit_button <- shiny::renderUI({
      if (length(shiny::reactiveValuesToList(data_sources)) == 0) {
        return()
      }

      shiny::tagList(
        shiny::p(
          "When all needed data sources have been added, click the 'Load Data' button below to continue."
        ),
        actionButton(inputId = shiny::NS(id, "submit"), label = "Load Data")
      )
    })

    dat_obj <- shiny::reactiveVal()
    data_source <- shiny::reactiveVal()
    data_type <- shiny::reactiveVal()

    shiny::observe({
      all_data_sources <- shiny::reactiveValuesToList(data_sources)

      tmp <- lapply(
        all_data_sources,
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
            "BIRTHMO",
            "HANDED"
          )
        ) |>
        unique()

      ## For the handfull of visits that are present in both UDS-3 and UDS-4, choose UDS-4
      tmp <- tmp[
        tmp[,
          .(
            uds = uds,
            keep = ifelse(length(uds) > 1 & uds == "wadrc_uds3", FALSE, TRUE)
          ),
          by = c("NACCID", "VISITYR", "VISITMO", "VISITDAY")
        ],
        on = c("NACCID", "VISITYR", "VISITMO", "VISITDAY", "uds")
      ]

      tmp <- tmp[tmp$keep]
      tmp$keep <- NULL
      tmp$uds <- NULL

      dat_obj(tmp)

      if ("csv" %in% all_data_sources$data_source) {
        data_source("csv")
      } else if (length(unique(all_data_sources$data_source)) == 1) {
        data_source(all_data_sources$data_source[1])
      } else if ("redcap" %in% all_data_sources$data_source) {
        data_source("redcap")
      }

      data_type("wadrc")

      shiny::updateTextInput(inputId = "redcap_uri", value = NULL)
    }) |>
      shiny::bindEvent(input$submit)

    return(list(
      dat_obj = dat_obj,
      data_source = data_source,
      data_type = data_type
    ))
  })
}

#' @rdname dataSelectModule
#'
#' @export
dataSelectApp <- function() {
  development <- dir.exists("inst/shiny/www")

  if (development) {
    print("Development...")
  }

  shiny::addResourcePath(
    "www",
    ifelse(
      development,
      "inst/shiny/www",
      system.file("www", package = "NpsychAssessmentTool")
    )
  )

  ui <- bslib::page_fluid(
    shiny::tags$head(
      shiny::tags$script(
        src = "www/scripts.js"
      )
    ),
    dataSelectUI("dat_select"),
    shiny::actionButton("fetch_data", label = "Submit")
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 1000 * 1024^2)

    dat_obj <- reactiveVal()

    data_input <- dataSelectServer("dat_select")

    shiny::observe({
      dat_obj(data_input$dat_obj())

      shiny::showNotification(ui = paste(dim(dat_obj()), collapse = "; "))
    })
  }

  shiny::shinyApp(ui, server)
}
