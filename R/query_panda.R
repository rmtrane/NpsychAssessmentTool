#' Get biomarker data
#'
#' @description
#' Queries the [Panda](https://panda.medicine.wisc.edu) database to get biomarker data for
#' ADRC participants.
#'
#' @param adrc_ptid A single string. ADRC participant id for which we want to pull biomarker data.
#' @param api_key A single string. API key for panda database.
#' @param base_query_file A single string. Optional.
#'
#' @returns
#' A list of `data.table`s containing biomarker data, with names corresponding to the tables from
#' Panda that was queried.
#'
#' @keywords internal
get_biomarker_data <- function(
  adrc_ptid = "adrc00006",
  api_key,
  base_query_file = system.file(
    "json/panda_template.json",
    package = "NpsychAssessmentTool"
  )
) {
  missing_pkgs <- c("httr2", "jsonlite")[c(
    !rlang::is_installed("httr2"),
    !rlang::is_installed("jsonlite")
  )]

  if (length(missing_pkgs) > 0) {
    cli::cli_abort(
      "Please install {.pkg {missing_pkgs}} to pull data from Panda. You can use {.code install.packages(c(\"{paste0(missing_pkgs, collapse = '\", \"')}\"))} to do so."
    )
  }

  col_prefixes <- c(
    "PET Appointments" = "view_petscan_appts",
    "LP Appointments" = "view_lp_appts",
    "Participants" = "view_participants",
    "Local Roche CSF - Sarstedt freeze 2, cleaned" = "cg_csf_local_roche_sarstedt_freeze_2",
    "Local Roche CSF - Sarstedt freeze 3" = "cg_csf_local_roche_sarstedt_freeze_3",
    "Local Roche CSF - Sarstedt freeze, cleaned" = "cg_csf_local_roche_sarstedt_freeze",
    "NTK MultiObs - CSF analytes" = "cg_ntk_multiobs_for_sharing_20200310",
    "NTK2 MultiObs - CSF, 20230311" = "cg_ntk2",
    "MK6240_NFT_Rating" = "view_cg_mk6240_braak",
    "NAV4694 Visual Ratings" = "cg_nav4694_visual_ratings",
    "PIB Visual Rating 20180126" = "cg_pib_visual_ratings_20180126"
  )

  replace_in_colnames <- c(
    "_1_[^2]" = "_",
    "_xw" = "",
    "_derived" = "",
    "Abeta" = "ABeta",
    "ABeta_42" = "ABeta42"
  )

  my_query <- gsub(
    x = readLines(base_query_file),
    pattern = "adrc_ptid",
    replacement = paste0("'", adrc_ptid, "'")
  ) |>
    jsonlite::fromJSON()

  ## Build the request. First, set base URL
  my_request <- httr2::request(
    base_url = 'https://panda.medicine.wisc.edu/api/search/search'
  ) |>
    # Next, add authorization piece (this is where the API key is needed)
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
    ) |>
    # Finally, specify request method
    httr2::req_method("POST")

  ## We perform the request once for each table except for the Participants table.
  ## This is included in all queries.
  tables <- my_query$query$tables
  tables <- tables[
    !tables$name %in%
      c(
        "MRI Appointments",
        "PET Appointments",
        "LP Appointments",
        "Participants"
      ),
    "name"
  ]

  all_responses <- lapply(tables, \(tab) {
    cur_query <- my_query

    cur_query$query$tables <- my_query$query$tables[
      my_query$query$tables$name %in% c(tab, "Participants"),
    ]

    ## If no date is in any table, add PET Appointments, and make table join "inner"
    all_names <- unlist(lapply(cur_query$query$tables$columns, `[[`, "name"))

    if (!any(grepl("date", tolower(all_names)))) {
      if (any(grepl("^age_lp$", tolower(all_names)))) {
        extra_table <- "LP Appointments"
      } else {
        extra_table <- "PET Appointments"
      }
      cur_query$query$tables <- my_query$query$tables[
        my_query$query$tables$name %in%
          c(tab, extra_table, "Participants"),
      ]
    }

    cur_query$query$tables$join[cur_query$query$tables$name == tab] <- "inner"

    cur_req <- my_request |>
      # Add the JSON to the body of the request.
      httr2::req_body_json(
        data = cur_query
      )

    cur_resp <- try(httr2::req_perform(cur_req), TRUE)

    if (inherits(cur_resp, "try-error")) {
      return(cur_resp)
    }

    if (cur_resp$status != 200) {
      return(cur_resp$status)
    }

    httr2::resp_body_json(cur_resp)$data
  }) |>
    setNames(tables)

  purrr::imap(all_responses, \(x, idx) {
    if (is.na(x) | inherits(x, "try-error")) {
      return(x)
    }

    if (x == "[]") {
      return(NULL)
    }

    as_df <- data.table::data.table(jsonlite::fromJSON(x))
    as_df$view_participants_adrcnum <- NULL

    age_cols <- grep("age", colnames(as_df), value = TRUE)

    if (length(age_cols) > 1) {
      as_df <- as_df[which(as_df[[age_cols[1]]] == as_df[[age_cols[2]]])]
      as_df[[age_cols[1]]] <- NULL
    }

    ## Fix binary variables to 0/1
    as_df[,
      names(.SD) := lapply(.SD, \(y) {
        y[y %in% c("TRUE", "true", "True", "Y")] <- 1
        y[y %in% c("FALSE", "false", "False", "N")] <- 0

        y[!y %in% c(0, 1)] <- NA

        as.numeric(y)
      }),
      .SDcols = grep(pattern = "_bin$|braak", x = names(as_df), value = T)
    ]

    ## Fix character columns that should be numeric
    as_df[,
      names(.SD) := lapply(.SD, \(y) {
        y[y %in% c("NA", "nan")] <- NA

        if (all(grepl("^\\d*\\.?\\d*$", y) | is.na(y))) {
          y <- as.numeric(y)
        }

        y
      }),
      .SDcols = is.character
    ]

    ## Fix column names
    data.table::setnames(
      as_df,
      names(as_df),
      new = gsub(
        pattern = paste(
          c(
            paste0("^", col_prefixes[idx], "_"),
            "view_petscan_appts_",
            "view_lp_appts_"
          ),
          collapse = "|"
        ),
        replacement = "",
        colnames(as_df)
      )
    )

    for (i in seq_along(replace_in_colnames)) {
      # For i = 1, we want to match on expression, but replace different expression. Hence the extra gsub in new.
      data.table::setnames(
        as_df,
        old = grep(
          pattern = names(replace_in_colnames)[i],
          x = names(as_df),
          value = TRUE
        ),
        new = gsub(
          gsub("[^2]", "", names(replace_in_colnames)[i], fixed = T),
          replace_in_colnames[i],
          grep(
            pattern = names(replace_in_colnames)[i],
            x = names(as_df),
            value = TRUE
          )
        )
      )
    }

    for (i in colnames(as_df)) {
      if (paste0(i, "_bin") %in% colnames(as_df)) {
        colnames(as_df)[colnames(as_df) == i] <- paste0(i, "_raw")
      }
    }

    as_df
  })
}


#' Format a table for gt
#'
#' @description
#' Takes a table as those in the entries of the list provided by `get_biomarker_data`,
#' and formats this for "pretty printing" using `gt::gt`.
#'
#' @param tab A `data.table`.
#'
#' @returns
#' A `data.table` with modified column names and values, formatted for use with `gt::gt`.
#' Returns `NULL` if the input `tab` is `NULL`.
#'
#' @keywords internal
bio_tab_for_gt <- function(tab) {
  if (inherits(tab, "try-error")) {
    return(tab)
  }

  if (is.null(tab)) {
    return(data.table::data.table(name = "No values found"))
  }

  if (!inherits(tab, "data.table")) {
    cli::cli_abort(
      "{.arg tab} must be of class {.cls data.table}, but is of class {.cls {class(tab)}}"
    )
  }

  colnames(tab)[grepl(pattern = "date", colnames(tab))] <- "date"
  colnames(tab)[grepl(pattern = "age", colnames(tab))] <- "Age_raw"

  tab[, date := as.Date(date)]

  if (!"date" %in% colnames(tab)) {
    tab$date <- paste("Visit", seq_along(tab$age_raw))
  }

  if (any(grepl("_bin$", x = names(tab)))) {
    tab <- data.table::melt(
      tab,
      id.vars = c("date"),
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|bin)"
      )
    )

    tab[,
      c("name", "raw", "bin") := list(
        factor(name, levels = unique(name)),
        ifelse(
          name == "Age",
          raw,
          ifelse(
            bin,
            '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i> Positive',
            '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i> Negative'
          )
        ),
        NULL
      )
    ]

    tab <- data.table::dcast(
      tab,
      name ~ date,
      value.var = "raw"
    )

    tab$name <- as.character(tab$name)
  }

  if ("rating_0_1_2_3" %in% names(tab)) {
    tab <- data.table::melt(
      tab,
      id.vars = c("date"),
      measure.vars = c("Age_raw", "rating_0_1_2_3")
    )

    tab[,
      c("variable", "value") := list(
        gsub(
          "rating_0_1_2_3",
          "PiB Visual Rating",
          x = gsub("_raw", "", x = variable)
        ),
        ifelse(
          variable == "Age_raw",
          value,
          c(
            "0" = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i> Clearly negative (0)',
            "1" = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i> Clearly negative (1)',
            "2" = "<span style='color: grey;'> Ambiguous/Indeterminate<span>",
            "3" = '<i class="glyphicon glyphicon-plus-sign" style="color:red;"></i> Positive'
          )[as.character(value)]
        )
      )
    ]

    tab <- data.table::dcast(
      tab,
      variable ~ date,
      value.var = "value"
    )

    data.table::setnames(
      tab,
      "variable",
      "name"
    )
  }

  if ("braak_1" %in% colnames(tab)) {
    tab <- data.table::melt(
      tab,
      id.vars = "date"
    )

    tab[,
      c("variable", "value") := list(
        c(
          "Age",
          "Comment",
          "Stage I",
          "Stage II",
          "Stage III",
          "Stage IV",
          "Stage V",
          "Stage VI"
        )[match(
          tab$variable,
          c(
            "Age_raw",
            "comment",
            "braak_1",
            "braak_2",
            "braak_3",
            "braak_4",
            "braak_5",
            "braak_6"
          )
        )],
        # fmt: skip
        value = data.table::fcase(
          variable %in% c("Age_raw", "comment"), as.character(value),
          value == 1, '<i class="glyphicon glyphicon-plus-sign" style="color:red;"></i> Positive',
          value == 0, '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i> Negative'
        )
      )
    ]

    tab <- data.table::dcast(
      tab,
      variable ~ date,
      value.var = "value"
    )

    data.table::setnames(
      tab,
      "variable",
      "name"
    )
  }

  tab
}

#' Transform a table to a gt object
#'
#' @description
#' A short description...
#'
#' @param tab_for_gt A `data.table` or a `list` of `data.table`s.
#'
#' @returns
#' A `gt::gt` table. If `tab_for_gt` is a list, the names of the list are used as
#' grouping variable for table.
#'
#' @keywords internal
bio_tab_to_gt <- function(tab_for_gt) {
  if (is.list(tab_for_gt) | !inherits(tab_for_gt, "data.table")) {
    tab_for_gt <- tab_for_gt[!unlist(lapply(tab_for_gt, is.null))]

    tab_for_gt <- lapply(tab_for_gt, \(x) {
      if (inherits(x, "try-error")) {
        x <- data.table::data.table(
          name = x[1]
        )
      }

      x
    })

    if (!all(sapply(tab_for_gt, data.table::is.data.table))) {
      non_dts <- tab_for_gt[!sapply(tab_for_gt, data.table::is.data.table)]

      cli::cli_abort(
        "When {.arg tab_for_gt} is of class {.cls list}, all elements must be of class {.cls data.table}, but {.val {names(non_dts)}} {?is/are} of class {.cls {unlist(sapply(non_dts, class))}}"
      )
    }

    # if (all(sapply(tab_for_gt, \(x) inherits(x, "try-error")))) {
    #   all_errors <- sapply(tab_for_gt, \(x) x[1])

    #   errors_table <- data.frame(
    #     table = names(all_errors),
    #     name = unname(all_errors),
    #     first = ifelse(grepl("HTTP 403", all_errors), "Remember, Panda can only be accessed when on campus or through VPN.")
    #   )
    # }

    tab_for_gt <- data.table::rbindlist(
      tab_for_gt,
      fill = TRUE,
      idcol = "table"
    )

    visit_dates <- setdiff(names(tab_for_gt), c("name", "table"))

    tab_for_gt <- tab_for_gt[, c("table", "name", sort(visit_dates)), with = F]
  }

  group <- if ("table" %in% names(tab_for_gt)) "table"

  if (!is.null(group)) {
    age_rows <- tab_for_gt[tab_for_gt$name == "Age"]

    age_rows$table <- NULL
    age_rows <- unique(age_rows[,
      c("table", names(.SD)) := c(
        "",
        lapply(.SD, \(x) unique(na.omit(x)))
      )
    ])

    tab_for_gt <- data.table::rbindlist(
      list(age_rows, tab_for_gt[tab_for_gt$name != "Age"]),
      fill = TRUE
    )
  }

  gt::gt(
    tab_for_gt,
    rowname_col = "name",
    groupname_col = group
  ) |>
    gt::tab_stub_indent(
      rows = 1:nrow(tab_for_gt),
      indent = if (!is.null(group)) 4
    ) |>
    gt::fmt(
      fns = \(x) sapply(x, gt::html),
      rows = tab_for_gt$name != "Age"
    ) |>
    gt::fmt(
      fns = \(x) paste(floor(as.numeric(x)), "years"),
      rows = tab_for_gt$name == "Age",
      columns = -"name"
    ) |>
    gt::cols_align(align = "left") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "right",
          style = "hidden"
        ),
        gt::cell_text(align = "left")
      ),
      locations = gt::cells_stub()
    ) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::sub_missing()
}
