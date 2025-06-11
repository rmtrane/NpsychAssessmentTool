#' UI for shinyAssessmentApp
#'
#' @keywords internal
#'
#' @export
appUI <- function() {
  bslib::page_navbar(
    ## Header: add JS scripts, CSS, and spinner elements to be shown/hidden later.
    header = shiny::tagList(
      shiny::tags$head(
        shiny::tags$script(
          src = "www/scripts.js"
        ),
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "www/styles.css"
        )
      ),
      shiny::tags$div(id = "spinner", class = "loader"),
      shiny::tags$div(id = "spinner_overlay", class = "loader_overlay")
    ),
    theme = bslib::bs_theme(version = 5),
    title = "Npsych Scoring Application",
    id = "main_navbar",
    navbar_options = bslib::navbar_options(underline = TRUE),
    ######
    ## Panels
    ######
    ## Introduction Panel
    bslib::nav_panel(
      title = "Introduction",
      shiny::tags$iframe(
        src = "qmd/introduction.html",
        height = "100%",
        width = "100%"
      )
    ),
    ## Data Selection Panel
    bslib::nav_panel(
      title = "Data Selection",
      value = "dataSelect",
      bslib::layout_columns(
        max_height = 500,
        col_widths = c(-3, 6, -3),
        dataSelectUI("dataSelect")
      )
    ),
    ## Main Panels
    bslib::nav_panel(
      title = "Scoring Tables and Figures",
      value = "tables-and-figures",
      ## Create side bar with study ID selector, demographics table, and options (in accordion)
      bslib::page_sidebar(
        fillable = T,
        sidebar = bslib::sidebar(
          width = "325px",
          shiny::tagList(
            shiny::selectizeInput(
              inputId = "current_studyid",
              label = "Study IDs",
              choices = NULL,
              width = NULL,
              options = list(
                create = FALSE,
                maxOptions = 100,
                closeAfterSelect = TRUE
              )
            ),
            gt::gt_output("demographics_table_output")
          ),
          bslib::accordion(
            id = "options",
            open = FALSE,
            bslib::accordion_panel(
              title = "Options",
              shiny::actionButton(
                inputId = "update_colors",
                label = "Save"
              ),
              descriptionsUI("desc"),
              shiny::hr(),
              shiny::sliderInput(
                inputId = "main_table_pct",
                label = "Main Table Font Size (pct)",
                value = 80,
                min = 1,
                max = 150
              ),
              shiny::hr(),
              shiny::checkboxInput(
                inputId = "shade_descriptions",
                label = "Shade according to descriptions?",
                value = T
              ),
              shiny::hr(),
              shiny::checkboxInput(
                inputId = "devmode",
                label = "Show number of visits with Study IDs?",
                value = F
              )
            )
          )
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
              shiny::span(
                class = "inline-input-container",
                shiny::tags$label("Visit Date", `for` = "current_date"),
                shiny::div(
                  class = "shiny-input-container",
                  shiny::selectizeInput(
                    inputId = "current_date",
                    label = NULL,
                    choices = NULL,
                    width = NULL,
                    options = list(
                      ## When new options loaded, resize the dropdown.
                      onLoad = I("resizeSelectize('current_date')")
                    )
                  )
                )
              ),
              mainTableUI("main_table"),
              fillable = T,
              gap = "0px"
            )
          ),
          bslib::card(
            id = "main-plot",
            full_screen = T,
            bslib::card_header("Longitudinal Trends"),
            bslib::navset_card_underline(
              bslib::nav_panel(
                title = "Cognitive Scores (Plots)",
                plotVarUI("plot_var")
              ),
              bslib::nav_panel(
                title = "Cognitive Scores (Table)",
                longTableUI("long_table")
              ),
              bslib::nav_panel(
                title = "Diagnoses",
                prevDiagnosesUI("prev_diagnoses_table")
              ),
              bslib::nav_panel(
                title = "Biomarkers",
                shiny::h6("Coming soon...")
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Setup",
      value = "colSelect",
      colSelectUI("colSelect")
    )
  )
}
