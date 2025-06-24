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
    class = "align-items-center",
    shiny::selectizeInput(
      inputId = ns("data_source"),
      label = "Data",
      choices = c(
        "Pull From REDCap" = "redcap",
        "Upload CSV File" = "csv_upload",
        "Demo" = "demo"
      ) # ,
      # width = "300px"
    ),
    shiny::uiOutput(ns("data_type_ui")),
    shiny::uiOutput(ns("data_upload_or_password")),
    shiny::uiOutput(ns("fetch_data")),
    min_height = "400px"
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
          label = "Source of data",
          choices = c(
            # "NACC" = "nacc",
            # "WLS" = "wls",
            "WADRC" = "wadrc_uds3" 
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
        choices = list(
          demo = c("NACC" = "nacc"),
          redcap = c(
            "WADRC" = "wadrc_uds3" #,
            # "WLS" = "wls"
          ),
          csv_upload = c(
            "NACC" = "nacc",
            "WLS" = "wls",
            "WADRC" = "wadrc_uds3"
          )
        )[[input$data_source]],
        selected = if (input$data_source == "demo") "nacc"
      )
    })

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
              value = uri_and_token$redcap_uri
            ),
            .cssSelector = "input",
            autocomplete = "username"
          ),
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "api_token"),
              label = "REDCap API Token",
              value = uri_and_token$token
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
        # shiny::actionButton(
        bslib::input_task_button(
          # inputId =
          id = shiny::NS(id, "fetch_data_button"),
          label = "Go",
          width = "300px"
        )
      }
    })

    dat_obj <- shiny::reactiveVal()

    shiny::observe({
      if (input$data_source == "redcap") {
        if (input$data_type == "wadrc_uds3") {
          shiny::removeNotification(id = "redcap_url_api_found")

          shiny::showNotification(
            "Pulling data from REDCap...",
            duration = NULL,
            type = "message",
            id = "pulling_from_redcap"
          )

          from_redcap <- data.table::data.table(
            REDCapR::redcap_read_oneshot(
              redcap_uri = input$redcap_uri,
              token = input$api_token,
              fields = wadrc_redcap_fields
            )$data
          )

          shiny::removeNotification(id = "pulling_from_redcap")

          shiny::showNotification(
            "Preparing REDCap data...",
            duration = NULL,
            type = "message",
            id = "preparing_from_redcap"
          )

          from_redcap <- wadrc_data_prep(
            adrc_data = from_redcap
          )

          shiny::removeNotification(id = "preparing_from_redcap")
          shiny::showNotification(
            "REDCap data successfully pulled and prepared!",
            duration = 3,
            type = "message"
          )

          dat_obj(from_redcap)
        }
      }

      if (input$data_source == "csv_upload") {
        dat_obj(
          data.table::fread(
            file = input$input_file$datapath,
            na.strings = c("", "NA")
          )
        )

        if (input$data_type == "wadrc_uds3") {
          dat_obj(
            wadrc_data_prep(dat_obj())
          )
        }
      }

      if (input$data_source == "demo") {
        dat_obj(
          demo_data
        )
      }
    }) |>
      shiny::bindEvent(input$fetch_data_button)

    return(list(
      dat_obj = dat_obj,
      data_type = shiny::reactive(input$data_type),
      data_source = shiny::reactive(input$data_source)
    ))
  })
}

#' @rdname dataSelectModule
#'
#' @export
dataSelectApp <- function() {
  development <- dir.exists("inst/shiny/www")

  if (development) print("Development...")

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
    dataSelectUI("dat_select")
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 1000 * 1024^2)

    dat_obj <- dataSelectServer("dat_select")

    # shiny::observe(
    #   print(head(dat_obj()))
    # )
  }

  shiny::shinyApp(ui, server)
}
