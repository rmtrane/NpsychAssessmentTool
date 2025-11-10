# Transform a table to a gt object

A short description...

## Usage

``` r
bio_tab_to_gt(tab_for_gt)
```

## Arguments

- tab_for_gt:

  A `data.table` or a `list` of `data.table`s.

## Value

A [`gt::gt`](https://gt.rstudio.com/reference/gt.html) table. If
`tab_for_gt` is a list, the names of the list are used as grouping
variable for table.
