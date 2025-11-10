# Function to Prepare WADRC UDS-3 Data

Function to Prepare WADRC UDS-3 Data

## Usage

``` r
wadrc_data_prep(adrc_data, uds = c("uds2", "uds3", "uds4"))
```

## Arguments

- adrc_data:

  Data as gotten from WADRC REDCap database. Could be a complete
  download to .csv file, then read into R, or pulled directly from
  REDCap using the `REDCapR` package.

- uds:

  Spefify if data are from UDS-2, UDS-3 or UDS-4 database.
