# Shiny Module for Selecting Columns

This shiny module gives the user the option to manually map column names
to NACC variable names.

A short description...

## Usage

``` r
colSelectUI(id)

colSelectServer(
  id,
  col_names,
  default_methods,
  data_type,
  col_selection = c("enable", "disable", "hide")
)

colSelectApp(
  col_names,
  default_methods,
  data_type = c("nacc", "wls", "wadrc"),
  col_selection = "enable",
  testing = FALSE
)
```

## Arguments

- id:

  id to link shiny modules

- col_names:

  Column names.

- default_methods:

  Default methods.

- data_type:

  One of `"nacc"`, `"wls"`, or `"wadrc"`.

- col_selection:

  string; one of 'enable', 'disable', or 'hide'. If 'enable', allow user
  to select which columns should be used for each variable. If
  'disable', show columns used, but without the option to select. If
  'hide', hide the column.

- testing:

  logical; passed to `shiny::shinyApp(..., options = list(test.mode))`

## Value

A shiny app.
