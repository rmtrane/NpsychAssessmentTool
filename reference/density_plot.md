# Plot density

A short description...

## Usage

``` r
density_plot(
  obs = 30,
  dens = all_densities[[2]]$pTau_raw,
  cuts = all_cuts[[2]][name == "pTau", list(color, min_obs, max_obs)],
  height = 100,
  width = 400,
  new_id = NULL
)
```

## Arguments

- obs:

  A numeric value. Optional.

- dens:

  A list or data frame containing density `x` and `y` values.

- cuts:

  A numeric vector of cut points.

- height:

  A single numeric for the plot height. Optional.

- width:

  A single numeric for the plot width. Optional.

- new_id:

  String to use for elementId of plotly container to avoid randomly
  assigned IDs.

## Value

A `plotly` object. Returns `NULL` invisibly if `obs` is `NA`.
