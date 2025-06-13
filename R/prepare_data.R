#' Wrapper to prepare raw data
#'
#' This is a wrapper function that returns only the desired columns, adds
#' variables that can be derived to the data, and adds standardized scores.
#'
#' @param dat data set similar to the NACC data. For an example, see
#'   `?demo_data`.
#' @param selected_cols vector with columns to keep. If named, the names must
#'   correspond to NACC variable names and entries columns in the data set `dat`.
#' @param with_diags logical (default `TRUE`); should diagnoses be included?
#'
#' @inheritParams NpsychBatteryNorms::add_standardized_scores
#'
#' @examples prepare_data(demo_data)
#'
#' @export
prepare_data <- function(
  dat,
  selected_cols = c(
    "RACE",
    "CDRGLOB",
    "MOCATOTS",
    "MOCBTOTS",
    "TRAILA",
    "TRAILARR",
    "TRAILALI",
    "OTRAILA",
    "OTRLARR",
    "DIGFORCT",
    "DIGFORSL",
    "DIGBACCT",
    "DIGBACLS",
    "WAIS",
    "MINTTOTS",
    "ANIMALS",
    "VEG",
    "UDSVERTN",
    "UDSVERFC",
    "UDSVERLC",
    "UDSBENTC",
    "UDSBENTD",
    "CRAFTVRS",
    "CRAFTURS",
    "CRAFTDVR",
    "CRAFTDRE",
    "REY1REC",
    "REY2REC",
    "REY3REC",
    "REY4REC",
    "REY5REC",
    "REY6REC",
    "REYDREC",
    "REYTCOR",
    "TRAILB",
    "TRAILBLI",
    "TRAILBRR",
    "MOCACLOC",
    "MOCACLOH",
    "MOCACLON",
    "OTRAILB",
    "OTRLBRR",
    "OTRLBLI",
    "NACCGDS",
    "CDRSUM",
    "UDSBENRS",
    "NACCID",
    "SEX",
    "EDUC",
    "NACCAGE",
    "BIRTHYR",
    "VISITYR",
    "VISITMO",
    "VISITDAY",
    "NACCUDSD",
    "NACCMMSE",
    "BOSTON",
    "LOGIMEM",
    "MEMUNITS",
    "MEMTIME",
    "DIGIF",
    "DIGIFLEN",
    "DIGIB",
    "DIGIBLEN"
  ),
  methods = NULL,
  print_messages = F,
  with_diags = T
) {
  if (is.null(names(selected_cols))) {
    names(selected_cols) <- selected_cols
  }

  selected_cols <- selected_cols[!names(selected_cols) %in% c("", "(blank)")]

  other_cols <- intersect(
    c(
      with(
        diag_contr_pairs,
        sort(c(presump_etio_diag, contribution, na.omit(other)))
      ),
      unlist(lapply(
        list(
          NpsychBatteryNorms::calculate_fas,
          NpsychBatteryNorms::calculate_mocaclock,
          NpsychBatteryNorms::calculate_reyarec,
          var_labels
        ),
        formalArgs
      ))
    ),
    colnames(dat)
  )

  ## Remove other_cols that were included in selected_cols
  other_cols <- setdiff(other_cols, selected_cols)

  ## Final vector of columns to select WITH NAMES!!!
  cols_to_select <- c(selected_cols, other_cols)

  dat <- dat[, cols_to_select, with = F]

  ## Fix missing names
  names(cols_to_select) <- ifelse(
    names(cols_to_select) == "",
    cols_to_select,
    names(cols_to_select)
  )

  ## Change column names
  colnames(dat) <- setNames(names(cols_to_select), cols_to_select)[colnames(
    dat
  )]

  if (is.null(methods)) {
    # ... retrieve the default methods
    methods <- NpsychBatteryNorms::default_methods

    # Remove methods for things not present in supplied data
    methods <- methods[names(methods) %in% colnames(dat)]

    # Stop with
    stopifnot("No known variables found in the data" = length(methods) > 0)
  }

  dat[,
    names(.SD) := lapply(.SD, as.numeric),
    .SDcols = is.logical
  ]

  ## Remove 'empty' rows, i.e. rows with no
  dat <- dat[
    rowSums(
      !is.na(dat[,
        setdiff(
          colnames(dat),
          c(
            "NACCID",
            data.table::patterns("^VISIT|^BIRTH", cols = colnames(dat))
          )
        ),
        with = F
      ])
    ) >
      0
  ]

  if (!is.numeric(dat$SEX)) {
    if (all(tolower(dat$SEX) %in% c("m", "f"))) {
      dat$SEX <- as.numeric(tolower(dat$SEX) == "f") + 1
    }

    if (all(tolower(dat$SEX) %in% c("male", "female"))) {
      dat$SEX <- as.numeric(tolower(dat$SEX) == "female") + 1
    }
  }

  if (!"VISITDATE" %in% colnames(dat)) {
    dat$VISITDATE <- as.Date(ifelse(
      test = is.na(dat$VISITYR) | is.na(dat$VISITMO) | is.na(dat$VISITDAY),
      yes = NA,
      no = paste(dat$VISITYR, dat$VISITMO, dat$VISITDAY, sep = "-")
    ))
  }

  if (!"NACCAGE" %in% colnames(dat)) {
    dat$NACCAGE <- lubridate::time_length(
      dat$VISITDATE -
        as.Date(ifelse(
          test = is.na(dat$BIRTHYR) | is.na(dat$BIRTHMO),
          yes = NA,
          no = paste(dat$BIRTHYR, dat$BIRTHMO, 1, sep = "-")
        )),
      unit = "years"
    )
  }

  if (all(c("OTHCOG", "OTHCOGX") %in% colnames(dat))) {
    dat$OTHCOG <- with(
      dat,
      ifelse(OTHCOG == 1, as.character(OTHCOGX), as.character(OTHCOG))
    )
  }

  if (all(c("OTHPSY", "OTHPSYX") %in% colnames(dat))) {
    dat$OTHPSY <- with(
      dat,
      ifelse(OTHPSY == 1, as.character(OTHPSYX), as.character(OTHPSY))
    )
  }

  if (all(c("COGOTH", "COGOTHX") %in% colnames(dat))) {
    dat$COGOTH <- with(
      dat,
      ifelse(COGOTH == 1, as.character(COGOTHX), as.character(COGOTH))
    )
  }

  if (all(c("COGOTH2", "COGOTH2X") %in% colnames(dat))) {
    dat$COGOTH2 <- with(
      dat,
      ifelse(
        COGOTH2 == 1,
        as.character(COGOTH2X),
        as.character(COGOTH2)
      )
    )
  }

  if (all(c("COGOTH3", "COGOTH3X") %in% colnames(dat))) {
    dat$COGOTH3 <- with(
      dat,
      ifelse(
        COGOTH3 == 1,
        as.character(COGOTH3X),
        as.character(COGOTH3)
      )
    )
  }

  diag_renames <- c(
    setNames(
      paste0(diag_contr_pairs$presump_etio_diag, "_etiology"),
      diag_contr_pairs$presump_etio_diag
    ),
    setNames(
      paste0(diag_contr_pairs$presump_etio_diag, "_contribution"),
      nm = diag_contr_pairs$contribution
    )
  )

  colnames(dat)[
    colnames(dat) %in% names(diag_renames)
  ] <- diag_renames[colnames(dat)[colnames(dat) %in% names(diag_renames)]]

  dat <- NpsychBatteryNorms::add_derived_scores(dat)

  ## Check that raw scores are numeric. If not, try to convert.
  ## Check if any raw_ columns are NOT numeric
  not_numeric_raw <- !unlist(dat[,
    lapply(.SD, is.numeric),
    .SDcols = names(methods)
  ])

  if (any(not_numeric_raw)) {
    not_numeric_raw <- names(which(not_numeric_raw))

    for (cur_raw in not_numeric_raw) {
      cur_raw_scores <- dat[[cur_raw]]

      digits_only <- grepl(
        "^[0-9]*\\.?[0-9]*$",
        na.omit(cur_raw_scores),
        perl = TRUE
      )

      if (!all(digits_only)) {
        cli::cli_abort(
          message = "'raw_scores' for {cur_raw} must be a numeric vector. You provided a vector of class {class(cur_raw_scores)}, which contains some non-digit characters, and therefore could not be converted."
        )
      }

      cur_raw_scores_numeric <- as.numeric(cur_raw_scores)

      ## Check that no values were converted to NA
      if (sum(is.na(cur_raw_scores)) == sum(is.na(cur_raw_scores_numeric))) {
        cli::cli_alert_warning(
          text = "'raw_scores' for {cur_raw} must be a numeric vector. You provided a {class(cur_raw_scores)}, but it was successfully converted to numeric."
        )

        dat[[cur_raw]] <- cur_raw_scores_numeric
      } else {
        cli::cli_abort(
          message = "'raw_scores' for {cur_raw} must be a numeric vector. You provided a vector of class {class(cur_raw_scores)}. No non-digit characters were found, but converting to numeric using `as.numeric` failed."
        )
      }
    }
  }

  dat <- NpsychBatteryNorms::add_standardized_scores(
    as.data.frame(dat),
    methods = methods,
    rename_raw_scores = T,
    delay = if ("MEMTIME" %in% colnames(dat)) "MEMTIME" else NULL,
    print_messages = print_messages
  )

  data.table::data.table(dat)
}
