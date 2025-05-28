#' Summary Table with Functional Measures
#'
#' @param dat Data set to use; see `demo_data` for example
#' @param orientation String specifying if dates should be displayed
#'   horizontally as columns ("h") or vertically as rows ("v")
#' @param age string specifying column containing age
#'
#' @examples
#' prep_dat <- prepare_data(demo_data)
#' functional_measures_table(
#'   dat = prep_dat[prep_dat$NACCID == prep_dat$NACCID[1],]
#' )
#'
#' @export

functional_measures_table <- function(
  dat,
  age,
  orientation = "h"
) {
  stopifnot("More than one study ID in data" = length(unique(dat$NACCID)) == 1)

  cols_to_use <- c(
    `FAS:` = "FAS",
    `Informant:` = "IQCODEINFORM",
    `Self:` = "IQCODESELF",
    `Global:` = "CDRGLOB",
    `SOB:` = "CDRSUM"
  )

  if (!(missing(age) || identical(age, quote(expr = )))) {
    cols_to_use <- c("Age" = age, cols_to_use)
  }

  for_table <- dat[order(dat$VISITDATE)]
  for_table$visit <- paste("Visit", order(dat$VISITDATE))
  for_table <- for_table[,
    c("visit", intersect(cols_to_use, colnames(for_table))),
    with = F
  ]
  colnames(for_table)[colnames(for_table) %in% cols_to_use] <- names(
    cols_to_use
  )

  if (orientation == "h") {
    for_table <- for_table[, setdiff(colnames(for_table), "Age"), with = F]
    for_table <- data.table::melt(
      for_table,
      id.vars = "visit",
      variable.name = "name"
    )
    for_table$visit <- with(
      for_table,
      factor(
        visit,
        levels = paste("Visit", 1:length(unique(visit)))
      )
    )
    for_table <- data.table::dcast(for_table, formula = name ~ visit)

    out <- for_table |>
      gt::gt(rowname_col = "name") |>
      gt::tab_header(
        gt::md("**Functional Measures**")
      ) |>
      gt::sub_missing() |>
      gt::tab_row_group(
        label = gt::md("<u>CDR</u>"),
        rows = .data$name %in% c("Global:", "SOB:"),
        id = "2"
      ) |>
      gt::tab_row_group(
        label = gt::md("<u>IQ-CODES</u>"),
        rows = grepl("Informant|Self", x = .data$name), #stringr::str_detect(.data$name, "Informant|Self"), # c(2,3),
        id = "3"
      ) |>
      gt::row_group_order(
        groups = c(NA, "2", "3")[c(
          any(
            #!stringr::str_detect(for_table$name, "Global|SOB|Informant|Self")
            !grepl("Global|SOB|Informant|Self", x = for_table$name)
          ),
          any(
            # stringr::str_detect(for_table$name, "Global|SOB")
            grepl("Global|SOB", x = for_table$name)
          ),
          any(
            #stringr::str_detect(for_table$name, "Informant|Self")
            grepl("Informant|Self", x = for_table$name)
          )
        )]
      ) |>
      gt::cols_align("center") |>
      gt::cols_align(
        "right",
        "name"
      ) |>
      gt::fmt_markdown() |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("left"),
          style = "hidden"
        ),
        location = gt::cells_body()
      ) |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("top"),
          style = "hidden"
        ),
        location = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("top"),
          style = "solid",
          weight = gt::px(2),
          color = "lightgrey"
        ),
        location = gt::cells_row_groups(c("2", "3")[c(
          any(
            # stringr::str_detect(for_table$name, "Global|SOB")
            grepl("Global|SOB", x = for_table$name)
          ),
          any(
            # stringr::str_detect(for_table$name, "Informant|Self")
            grepl("Informant|Self", x = for_table$name)
          )
        )])
      ) |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = "bottom",
          style = "hidden"
        ),
        locations = list(
          gt::cells_body(
            rows = .data$name %in% c("Global:", "Informant:")
          ),
          gt::cells_stub(
            rows = .data$name %in% c("Global:", "Informant:")
          ),
          gt::cells_row_groups(
            groups = c("2", "3")[c(
              any(
                #stringr::str_detect(for_table$name, "Global|SOB")
                grepl("Global|SOB", x = for_table$name)
              ),
              any(
                # stringr::str_detect(for_table$name, "Informant|Self")
                grepl("Informant|Self", x = for_table$name)
              )
            )]
          )
        )
      ) |>
      gt::tab_options(
        row_group.border.top.style = "solid",
        table.border.left.style = "solid",
        table.border.right.style = "solid",
        # table_body.border.bottom.style = 'hidden',
        heading.title.font.size = gt::px(16),
        row_group.font.size = gt::px(16),
        table.font.size = gt::px(16),
        table.width = gt::px(275)
      )
  }

  if (orientation == "v") {
    if (nrow(for_table) > 1) {
      colnames(for_table) <- gsub(
        pattern = ":$",
        replacement = "",
        x = colnames(for_table)
      )

      for_out <- gt::gt(for_table, id = "fastable", rowname_col = "visit")
    } else {
      for_table <- for_table[, -"visit"]
      colnames(for_table) <- gsub(
        pattern = ":$",
        replacement = "",
        x = colnames(for_table)
      )

      for_out <- gt::gt(for_table, id = "fastable")
    }

    out <- for_out |>
      gt::tab_spanner(
        label = "CDR",
        columns = intersect(c("Global", "SOB"), colnames(for_out))
      ) |>
      gt::tab_spanner(
        label = "IQCODES",
        columns = intersect(c("Informant", "Self"), colnames(for_out))
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_borders(
            style = "hidden"
          )
        ),
        locations = list(
          gt::cells_body(),
          gt::cells_column_spanners(),
          gt::cells_column_labels()
        )
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(align = "center")
        ),
        locations = gt::cells_body()
      ) |>
      gt::tab_style(
        style = list(gt::cell_text(weight = "bold")),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = list(gt::cell_text(style = "italic")),
        locations = gt::cells_column_spanners()
      )
  }

  out
}
