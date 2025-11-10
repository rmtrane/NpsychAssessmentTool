# Convert biomarker data table to HTML table

A short description...

## Usage

``` r
bio_tab_to_html_table(
  tab_for_gt,
  densities,
  cuts,
  www_path = if (dir.exists("inst/www")) {
     "inst/www"
 } else {
    
    system.file("www", package = "NpsychAssessmentTool")
 },
  print_x = FALSE
)
```

## Arguments

- tab_for_gt:

  A list of `data.table`s, where each `data.table` is named after the
  Panda table that was queried to get the data.

- densities:

  A list of density data.

- cuts:

  A list or data structure containing cut-off values.

- www_path:

  Path to www/ folder that contains .css and .js files. Defaults to
  either inst/www or www subdir of NpsychAssessmentTool pacakge.

- print_x:

  For debugging

## Value

A
[`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
object representing the HTML table. Will error if `tab_for_gt` is not a
list of `data.table`s as expected.
