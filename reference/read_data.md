# Read data

Function used to read in data based on the output returned from
[`dataSelectServer()`](https://rmtrane.github.io/NpsychAssessmentTool/reference/dataSelectModule.md).

## Usage

``` r
read_data(data_source = "redcap", data_type = NULL, redcap_auth, data_file)
```

## Arguments

- data_source:

  One of `"redcap"`, `"csv_upload"`, or `"demo"`.

- data_type:

  Optional. If `data_source` is `"redcap"`, then must be one of
  `"wadrc_uds2"`, `"wadrc_uds3"` or `"wadrc_uds4"`.

- redcap_auth:

  Optional. A list with entries `redcap_uri` and `token` (when
  `data_source = "redcap"`).

- data_file:

  Optional. A character vector with path to data file (when
  `data_source = "csv"`).

## Value

A `data.table` object.
