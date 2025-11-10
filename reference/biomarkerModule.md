# Biomarker UI

A short description...

A short description...

A short description...

## Usage

``` r
biomarkerUI(id)

biomarkerServer(
  id,
  adrc_ptid,
  biomarker_api,
  base_query_file = system.file("json/panda_template.json", package =
    "NpsychAssessmentTool"),
  all_values
)

biomarkerApp(adrc_ptid, biomarker_api, all_values, testing = FALSE)
```

## Arguments

- id:

  A string used to namespace the module.

- adrc_ptid:

  ADRC ptid. A character vector with patient ID's that can be chosen.

- biomarker_api:

  Panda API key.

- all_values:

  A reactive value

## Value

A UI definition.

NULL.

A `shinyApp` object.
