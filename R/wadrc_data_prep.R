#' Function to Prepare WADRC UDS-3 Data
#'
#' @param adrc_data Data as gotten from WADRC UDS-3 REDCap.
#'  Could be a complete download to .csv file, then read into R,
#'  or pulled directly from REDCap using the `REDCapR` package.
#' @param uds Spefify if data are from UDS-3 or UDS-4 database.
#'
#' @keywords internal
wadrc_data_prep <- function(adrc_data, uds = c("uds3", "uds4")) {
  ## Due to NSE notes in R CMD check:
  NACCID <-
    VISITDATE <-
      NACCUDSD <-
        contributions_character <-
          var <-
            val <-
              for_tab <-
                visityr <-
                  visitmo <-
                    visitday <-
                      ptid <- NULL

  if (!data.table::is.data.table(adrc_data)) {
    cli::cli_abort(
      "The {.arg adrc_data} must be a {.cls data.table} object, but is a {.cls {class(adrc_data)}}."
    )
  }

  out <- adrc_data[
    !grepl(pattern = "biomarker", x = adrc_data$redcap_event_name)
  ]

  if (uds == "uds3") {
    prefixes <- c(
      "",
      "fu_",
      "tcog_",
      "tcog_wadrc_c2_",
      "wadrc_c2_",
      "tcog_wadrc_c22_",
      "wadrc_c22_",
      "tip_",
      "tele_"
    )

    suffixes <- c(
      "",
      "",
      "",
      "_v1_1",
      "_v1_1",
      "",
      "",
      "",
      ""
    )

    for_diagnosis <- c(
      "normcog",
      "demented",
      "mciamem",
      "mciaplus",
      "mcinon1",
      "mcinon2",
      "impnomci"
    )

    colnames(out)[grepl("education", colnames(out))] <- gsub(
      pattern = "education",
      replacement = "educ",
      x = colnames(out)[grepl("education", colnames(out))]
    )

    out <- out[,
      intersect(
        colnames(out),
        unique(c(
          "ptid",
          "redcap_event_name",
          data.table::patterns("^visit", cols = colnames(out)),
          paste0(
            prefixes,
            rep(c(nacc_to_wadrc_uds3, for_diagnosis), each = length(prefixes)),
            suffixes
          )
        ))
      ),
      with = F
    ]

    rm_cols <- colnames(out)[grepl(pattern = "_visit", colnames(out))]

    if (length(rm_cols) > 0) {
      out[, (rm_cols) := NULL]
    }

    ## Combine columns that all relate to same variables
    for (var in setdiff(
      c(nacc_to_wadrc_uds3, for_diagnosis),
      c(
        "ptid",
        "redcap_event_name",
        data.table::patterns("visit", cols = colnames(out))
      )
    )) {
      alt_cols <- setdiff(
        colnames(out)[grepl(
          pattern = paste0(
            c("^", "_", "^", "_"),
            var,
            c("$", "$", "_", "_"),
            collapse = "|"
          ),
          x = colnames(out)
        )],
        var
      )

      out[,
        (var) := apply(.SD, 1, return_single, if_multiple = -1),
        .SDcols = intersect(c(var, alt_cols), colnames(out))
      ]
      out[, (alt_cols) := NULL]
    }
  }

  if (uds == "uds4") {
    # Keep only visits related to uds4
    out <- out[
      # !grepl(pattern = "biomarker", x = out$redcap_event_name)
      !grepl(pattern = "uds3", x = out$redcap_event_name)
    ]
    # Create visityr, visitmo, visitday
    out$visityr = lubridate::year(out$visitdate)
    out$visitmo = lubridate::month(out$visitdate)
    out$visitday = lubridate::day(out$visitdate)

    # Rename 'birthsex' to 'sex'
    colnames(out)[colnames(out) == "birthsex"] <- "sex"

    # Create 'race' column
    out$race <- NA

    out$visitdate <- NULL
  }

  out <- out[!(is.na(visityr) & is.na(visitmo) & is.na(visitday))]

  ## Remove redcap_event_name
  out$redcap_event_name <- NULL # out[, redcap_event_name := NULL]
  out <- unique(out)

  ## Create diagnosis variable udsd
  out$udsd <- NA

  if (uds == "uds3") {
    out$udsd[out$normcog == 1] <- 1
    out$udsd[out$impnomci == 1] <- 2
    out$udsd[
      out$mciamem == 1 |
        out$mciaplus == 1 |
        out$mcinon1 == 1 |
        out$mcinon2 == 1
    ] <- 3
    out$udsd[out$demented == 1] <- 4
  }

  if (uds == "uds4") {
    out$udsd[out$normcog == 1] <- 1
    out$udsd[out$impnomci == 1] <- 2
    out$udsd[out$mci == 1] <- 3
    out$udsd[out$demented == 1] <- 4
  }

  out <- fill_data_downup(out)

  ## Rename all columns to match NACC naming scheme. To do so, create
  ## character vector of the form c("new_name" = "old_name", ...)

  ## For UDS3, this is exactly nacc_to_wadrc_uds3
  if (uds == "uds3") {
    cols_wanted <- nacc_to_wadrc_uds3
  }

  ## For UDS4, need to add visityr, visitmo, visitday, and sex, while removing
  ## visitdate and birthsex.
  if (uds == "uds4") {
    cols_wanted <- c(
      "VISITYR" = "visityr",
      "VISITMO" = "visitmo",
      "VISITDAY" = "visitday",
      "SEX" = "sex",
      nacc_to_wadrc_uds4[!nacc_to_wadrc_uds4 %in% c("birthsex", "visitdate")]
    )
  }

  out <- setNames(
    out[, cols_wanted, with = F],
    nm = names(cols_wanted)
  )

  out
}

collapse_vals <- \(x) {
  no_nas <- na.omit(unique(x))

  if (length(no_nas) == 0) {
    return(NA)
  }

  if (length(no_nas) > 1) {
    return(x)
  }

  no_nas
}


return_single <- function(x, if_multiple = NA) {
  if (all(is.na(x))) {
    return(NA)
  }

  x <- unique(na.omit(x))

  if (length(x) > 1) {
    return(if_multiple)
  }

  x
}


fill_data_downup <- function(
  out,
  ptid = "ptid",
  visityr = "visityr",
  visitmo = "visitmo",
  visitday = "visitday",
  educ = "educ",
  constant_across_visits = c("sex", "race", "birthyr", "birthmo", "handed")
) {
  remove_after <- c()
  if (ptid != "ptid") {
    out$ptid <- out[[ptid]]
    remove_after <- c(remove_after, "ptid")
  }
  if (visityr != "visityr") {
    out$visityr <- out[[visityr]]
    remove_after <- c(remove_after, "visityr")
  }
  if (visitmo != "visitmo") {
    out$visitmo <- out[[visitmo]]
    remove_after <- c(remove_after, "visitmo")
  }
  if (visitday != "visitday") {
    out$visitday <- out[[visitday]]
    remove_after <- c(remove_after, "visitday")
  }
  if (educ != "educ") {
    out$educ <- out[[educ]]
    remove_after <- c(remove_after, "educ")
  }

  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[, names(.SD) := lapply(.SD, as.numeric), .SDcols = is.logical]

  ## Fill NA within visits downup (i.e. first fill down, then fill up).
  ## For numeric values, use data.table::nafill directly
  out[,
    names(.SD) := lapply(
      .SD,
      \(x) {
        # If x of length 1, simply return x (there is nothing to fill...)
        if (length(x) == 1) {
          return(x)
        }

        data.table::nafill(x, type = "locf") |>
          data.table::nafill(type = "nocb")
      }
    ),
    .SDcols = is.numeric,
    by = c("ptid", "visityr", "visitmo", "visitday")
  ]

  ## For character columns, need workaround. First, make factor
  char_cols <- setdiff(
    colnames(out)[sapply(out, is.character)],
    c("ptid", "visityr", "visitmo", "visitday")
  )

  if (length(char_cols) > 0) {
    out[, (char_cols) := lapply(.SD, factor), .SDcols = char_cols]
    ## Next, get levels of factors (for later refill)
    lev = sapply(char_cols, function(x) levels(out[[x]]))
    ## Now, make integer...
    out[, (char_cols) := lapply(.SD, as.integer), .SDcols = char_cols]
    ## ... then fill
    out[,
      (char_cols) := lapply(
        .SD,
        \(x) {
          if (length(x) == 1) {
            return(x)
          }

          data.table::nafill(x, 'locf') |>
            data.table::nafill(type = "nocb")
        }
      ),
      by = c("ptid", "visityr", "visitmo", "visitday"),
      .SDcols = char_cols
    ]

    ## Now, refill with levels to return to characters
    for (col in char_cols) {
      data.table::set(out, NULL, col, lev[[col]][out[[col]]])
    }
  }

  ## Remove duplicate rows if any
  out <- unique(out)

  ## Next we want to fill in sex, educ, race, handed, birthyr, birthmo by ptid
  ## First, make sure data.table is sorted appropriately
  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[,
    names(.SD) := lapply(.SD, \(x) {
      if (length(x) == 1) {
        return(x)
      }

      data.table::nafill(x, type = "locf") |>
        data.table::nafill(type = "nocb")
    }),
    .SDcols = intersect(colnames(out), constant_across_visits),
    # .SDcols = data.table::patterns(
    #   c("sex", "race", "birthyr", "birthmo", "handed"),
    #   cols = colnames(out)
    # ),
    by = "ptid"
  ]

  ## Remove duplicate rows if any
  out <- unique(out)

  ## Finally, if only a single non-missing educ value is present,
  ## copy to all entries. Otherwise, fill downup.
  out[,
    educ := {
      if (!all(is.na(educ))) {
        nonas <- na.omit(unique(educ))

        if (length(nonas) == 1) {
          educ <- rep(nonas, length(educ))
        }

        data.table::nafill(educ, type = "locf") |>
          data.table::nafill(type = "nocb")
      } else {
        educ
      }
    },
    by = "ptid"
  ]

  for (rm_var in remove_after) {
    out[[rm_var]] <- NULL
  }

  return(out)
}
