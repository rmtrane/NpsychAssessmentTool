# Shiny module to select data for assessment table

A short description...

Server logic for dataSelectModule.

## Usage

``` r
dataSelectUI(id)

dataSelectServer(id)

dataSelectApp(testing = FALSE)
```

## Arguments

- id:

  A string used to create a namespace within the shiny app.

- testing:

  Logical, whether to run the app in testing mode.

## Value

A UI definition.

A list containing reactive values for the data object (`dat_obj`), the
data source, the data type, and the biomarker API.
