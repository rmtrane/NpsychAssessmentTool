# Format a table for gt

Takes a table as those in the entries of the list provided by
`get_biomarker_data`, and formats this for "pretty printing" using
[`gt::gt`](https://gt.rstudio.com/reference/gt.html).

## Usage

``` r
bio_tab_for_gt(tab, return = c("binary", "raw", "both"))
```

## Arguments

- tab:

  A `data.table`.

## Value

A `data.table` with modified column names and values, formatted for use
with [`gt::gt`](https://gt.rstudio.com/reference/gt.html). Returns
`NULL` if the input `tab` is `NULL`.
