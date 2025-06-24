#' Function to Prepare WADRC Data
#'
#' @param adrc_data Data as gotten from WADRC REDCap.
#'  Could be a complete download to .csv file, then read into R,
#'  or pulled directly from REDCap using the `REDCapR` package.
#' @export
wadrc_data_prep <- function(adrc_data) {
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

  stopifnot(
    "'adrc_data' must be a data.table object" = data.table::is.data.table(
      adrc_data
    )
  )

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

  out <- adrc_data[
    !grepl(pattern = "biomarker", x = adrc_data$redcap_event_name)
  ]

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

  if (length(rm_cols) > 0) out[, (rm_cols) := NULL]

  # out_ <- out
  # out <- out_

  # out <- data.table::melt(
  #   out[, lapply(.SD, as.character)],
  #   id.vars = c("ptid", "redcap_event_name", data.table::patterns("visit", cols = colnames(out))),
  #   measure.vars = data.table::measure(

  #   )
  # )

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

  out <- out[!(is.na(visityr) & is.na(visitmo) & is.na(visitday))]

  ## Remove redcap_event_name
  out$redcap_event_name <- NULL # out[, redcap_event_name := NULL]
  out <- unique(out)

  ## Create diagnosis variable udsd
  out$udsd <- NA
  out$udsd[out$normcog == 1] <- 1
  out$udsd[out$impnomci == 1] <- 2
  out$udsd[
    out$mciamem == 1 |
      out$mciaplus == 1 |
      out$mcinon1 == 1 |
      out$mcinon2 == 1
  ] <- 3
  out$udsd[out$demented == 1] <- 4

  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[, names(.SD) := lapply(.SD, as.numeric), .SDcols = is.logical]

  ## Fill NA within visits downup (i.e. first fill down, then fill up).
  ## For numeric values, use data.table::nafill directly
  out[,
    names(.SD) := lapply(
      .SD,
      \(x) {
        # If x of length 1, simply return x (there is nothing to fill...)
        if (length(x) == 1) return(x)

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
        if (length(x) == 1) return(x)

        data.table::nafill(x, 'locf') |>
          data.table::nafill(type = "nocb")
      }
    ),
    by = c("ptid", "visityr", "visitmo", "visitday"),
    .SDcols = char_cols
  ]

  ## Now, refill with levels to return to characters
  for (col in char_cols) data.table::set(out, NULL, col, lev[[col]][out[[col]]])

  ## Remove duplicate rows if any
  out <- unique(out)

  ## Next we want to fill in sex, educ, race, birthyr, birthmo by ptid
  ## First, make sure data.table is sorted appropriately
  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[,
    names(.SD) := lapply(.SD, \(x) {
      if (length(x) == 1) return(x)

      data.table::nafill(x, type = "locf") |>
        data.table::nafill(type = "nocb")
    }),
    .SDcols = c("sex", "race", "birthyr", "birthmo"),
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

        if (length(nonas) == 1) educ <- rep(nonas, length(educ))

        data.table::nafill(educ, type = "locf") |>
          data.table::nafill(type = "nocb")
      } else {
        educ
      }
    },
    by = "ptid"
  ]

  # waldo::compare(
  #   data.table::data.table(out_old[, colnames(out)]),
  #   out
  # )

  # out_old <- adrc_data |>
  #   dplyr::filter(
  #     !stringr::str_detect(.data$redcap_event_name, "biomarker")
  #   ) |>
  #   dplyr::rename_with(
  #     \(x) stringr::str_replace_all(x, "education", "educ"),
  #     .cols = tidyselect::contains("education")
  #   ) |>
  #   dplyr::select(
  #     .data$ptid,
  #     .data$redcap_event_name,
  #     tidyselect::starts_with("visit"),
  #     tidyselect::any_of(paste0(
  #       prefixes,
  #       rep(c(nacc_to_wadrc_uds3, for_diagnosis), each = length(prefixes)),
  #       suffixes
  #     )),
  #     -tidyselect::contains("_visit")
  #   ) |>
  #   tidyr::pivot_longer(
  #     cols = -c(
  #       dplyr::all_of(c("ptid", "redcap_event_name")),
  #       tidyselect::contains("visit")
  #     ),
  #     names_pattern = paste0(
  #       "^(",
  #       paste(prefixes, collapse = "|"),
  #       ")(",
  #       paste(c(nacc_to_wadrc_uds3, for_diagnosis), collapse = "|"),
  #       ")(_v1_1|)$"
  #     ),
  #     names_to = c("type", ".value", "suffix")
  #   ) |>
  #   dplyr::select(-dplyr::all_of(c("type", "suffix", "redcap_event_name"))) |>
  #   dplyr::filter(
  #     !dplyr::if_any(c("visityr", "visitmo", "visitday"), is.na)
  #   ) |>
  #   dplyr::arrange(
  #     .data$ptid,
  #     .data$visityr,
  #     .data$visitmo,
  #     .data$visitday
  #   ) |>
  #   dplyr::group_by(
  #     .data$ptid,
  #     .data$visityr,
  #     .data$visitmo,
  #     .data$visitday
  #   ) |>
  #   tidyr::fill(
  #     tidyselect::everything(),
  #     .direction = "downup"
  #   ) |>
  #   dplyr::ungroup() |>
  #   unique() |>
  #   dplyr::group_by(.data$ptid) |>
  #   tidyr::fill(
  #     .data$sex,
  #     # .data$educ,
  #     .data$race,
  #     .data$birthyr,
  #     .data$birthmo,
  #     .direction = "downup"
  #   ) |>
  #   dplyr::ungroup() |>
  #   dplyr::mutate(
  #     udsd = dplyr::case_when(
  #       .data$normcog == 1 ~ 1,
  #       .data$impnomci == 1 ~ 2,
  #       .data$mciamem == 1 ~ 3,
  #       .data$mciaplus == 1 ~ 3,
  #       .data$mcinon1 == 1 ~ 3,
  #       .data$mcinon2 == 1 ~ 3,
  #       .data$demented == 1 ~ 4,
  #       .default = NA
  #     )
  #   ) |>
  #   dplyr::mutate(#summarize(
  #     .by = .data$ptid, # tidyselect::matches("[^educ]"),
  #     educ = return_single(.data$educ)
  #   ) |>
  #   dplyr::mutate(
  #     dplyr::across(dplyr::where(is.logical), \(x) {
  #       if (dplyr::cur_column() %in% nacc_to_wadrc_uds3) {
  #         return(as.numeric(x))
  #       }

  #       x
  #     })
  #   ) |>
  #   unique()

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
