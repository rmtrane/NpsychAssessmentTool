# Get biomarker data

Queries the [Panda](https://panda.medicine.wisc.edu) database to get
biomarker data for ADRC participants.

## Usage

``` r
get_biomarker_data(
  adrc_ptid = "adrc00006",
  api_key,
  base_query_file = system.file("json/panda_template.json", package =
    "NpsychAssessmentTool")
)
```

## Arguments

- adrc_ptid:

  A single string. ADRC participant id for which we want to pull
  biomarker data.

- api_key:

  A single string. API key for panda database.

- base_query_file:

  A single string. Optional.

## Value

A list of `data.table`s containing biomarker data, with names
corresponding to the tables from Panda that was queried.
