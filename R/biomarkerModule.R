#' Biomarker UI
#'
#' @description
#' A short description...
#'
#' @param id A string used to namespace the module.
#'
#' @returns
#' A UI definition.
#'
#' @rdname biomarkerModule
#'
#' @keywords internal
biomarkerUI <- function(id) {
  bslib::accordion(
    id = "biomarker-accordion",
    open = TRUE,
    bslib::accordion_panel(
      title = shiny::HTML('<strong>LP Visits</strong>'),
      gt::gt_output(shiny::NS(id, "lp_visits_table"))
    ),
    bslib::accordion_panel(
      title = shiny::HTML("<strong>PET Visits</strong>"),
      gt::gt_output(shiny::NS(id, "pet_visits_table"))
    )
  )
}

#' Biomarker server
#'
#' @description
#' A short description...
#'
#' @param id A string used to namespace the module.
#' @param adrc_ptid A reactive value specifying the ADRC patient ID.
#' @param biomarker_api A reactive value giving the Panda API token.
#' @param use_mirai A logical value specifying if `"mirai"` should be . Optional; defaults to `TRUE` if `"mirai"` is installed.
#'
#' @returns
#' NULL.
#'
#' @rdname biomarkerModule
#'
#' @export
biomarkerServer <- function(
  id,
  adrc_ptid,
  biomarker_api,
  use_mirai = rlang::is_installed("mirai")
) {
  ###################
  ## BEFORE SERVER

  ##
  ###################

  ###################
  ## START SERVER
  shiny::moduleServer(id, function(input, output, session) {
    # reactiveValues object to hold biomarker tables. This approach
    # lets us retrieve previously generated tables without fetching
    # the data from Panda again.
    biomarker_dat_tables <- shiny::reactiveValues()

    # If mirai is installed, we use ExtendedTask to get biomarker_dat in the background.
    if (rlang::is_installed("mirai") && use_mirai) {
      # Prepare to hold mirai object
      m <- NULL

      # Create ExtendedTask that will evaluate get_biomarker_data
      biomarker_dat <- shiny::ExtendedTask$new(
        \(cur_id, api) {
          m <<- mirai::mirai(
            {
              get_biomarker_data(
                adrc_ptid = cur_id,
                api_key = api
              )
            },
            .args = list(
              get_biomarker_data = get_biomarker_data,
              cur_id = cur_id,
              api = api
            )
          )

          m
        }
      )

      # When adrc_ptid or biomarker_api is updated, invoke the ExtendedTask
      shiny::observe({
        shiny::req(
          biomarker_api(),
          adrc_ptid()
        )

        # If the ExtendedTask is already running, stop it.
        if (biomarker_dat$status() == "running") {
          mirai::stop_mirai(m)
        }

        # Invoke, i.e. evaluate the ExtendedTask
        biomarker_dat$invoke(
          cur_id = adrc_ptid(),
          api = biomarker_api()
        )
      }) |>
        shiny::bindEvent(
          adrc_ptid(),
          biomarker_api()
        )

      shiny::observe({
        # If ExtendedTask successfully ran...
        if (biomarker_dat$status() == "success") {
          # ... and the table for the adrc_ptid has not already been saved
          if (!adrc_ptid() %in% names(biomarker_dat_tables)) {
            biomarker_dat_tables[[adrc_ptid()]] <- biomarker_dat$result()
          }
        }
      })
    } else {
      # If mirai is not installed, or we ask not to use mirai...
      if (FALSE) {
        # need to check if this works.
        shiny::observe({
          # If ExtendedTask successfully ran...
          # ... and the table for the adrc_ptid has not already been saved
          if (!adrc_ptid() %in% names(biomarker_dat_tables)) {
            biomarker_dat_tables[[adrc_ptid()]] <- get_biomarker_data(
              adrc_ptid = adrc_ptid(),
              api_key = biomarker_api()
            )
          }
        })
      }
    }

    # Table to present while getting biomarker data.
    loading_gt <- gt::gt(
      data = data.frame(x = '<span class="loading">Loading data</span>')
    ) |>
      gt::fmt(
        columns = "x",
        fns = \(x) gt::html(x)
      ) |>
      gt::cols_label(x = "") |>
      gt::opt_table_lines("none")

    # Table for LP visits
    output$lp_visits_table <- gt::render_gt(
      {
        if (adrc_ptid() %in% names(biomarker_dat_tables)) {
          lapply(
            biomarker_dat_tables[[adrc_ptid()]][c(
              "Local Roche CSF - Sarstedt freeze, cleaned",
              "Local Roche CSF - Sarstedt freeze 2, cleaned",
              "Local Roche CSF - Sarstedt freeze 3",
              "NTK MultiObs - CSF analytes",
              "NTK2 MultiObs - CSF, 20230311"
            )],
            bio_tab_for_gt
          ) |>
            bio_tab_to_gt()
        } else {
          loading_gt
        }
      },
      align = "left"
    )

    # Table for PET visits
    output$pet_visits_table <- gt::render_gt(
      {
        if (adrc_ptid() %in% names(biomarker_dat_tables)) {
          lapply(
            biomarker_dat_tables[[adrc_ptid()]][c(
              "MK6240_NFT_Rating",
              "NAV4694 Visual Ratings",
              "PIB Visual Rating 20180126"
            )],
            bio_tab_for_gt
          ) |>
            bio_tab_to_gt()
        } else {
          loading_gt
        }
      },
      align = "left"
    )
  })
}


#' Biomarker app
#'
#' @description
#' A short description...
#'
#' @param adrc_ptid ADRC ptid. A character vector with patient ID's that can be chosen.
#' @param biomarker_api Panda API key.
#'
#' @returns
#' A `shinyApp` object.
#'
#' @rdname biomarkerModule
#'
#' @export
biomarkerApp <- function(
  adrc_ptid,
  biomarker_api
) {
  ui <- bslib::page_fluid(
    shinyApp_header(),
    shiny::selectInput(
      inputId = "current_studyid",
      label = "ADRC ptid",
      choices = adrc_ptid
    ),
    biomarkerUI("biomarker-module")
  )

  server <- function(input, output, session) {
    biomarkerServer(
      "biomarker-module",
      adrc_ptid = shiny::reactive(input$current_studyid),
      biomarker_api
    )
  }

  shiny::shinyApp(ui, server)
}
