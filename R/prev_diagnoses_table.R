if (FALSE) {
  library(tidyverse)

  ## Create tibble to use for diagnoses
  short_descs <- map(
    rdd,
    \(x) {
      tibble(
        short_descriptor = pluck(x, "short_descriptor"),
        codes = list(pluck(x, "codes"))
      )
    }
  ) |>
    bind_rows(.id = "nacc_name")

  diag_contr_pairs <- tribble(
    ~presump_etio_diag,
    ~contribution,
    ~other,
    "ALCDEM",
    "ALCDEMIF",
    NA,
    "ANXIET",
    "ANXIETIF",
    NA,
    "BIPOLDX",
    "BIPOLDIF",
    NA,
    "BRNINJ",
    "BRNINJIF",
    NA,
    "COGOTH",
    "COGOTHIF",
    "COGOTHX",
    "COGOTH2",
    "COGOTH2F",
    "COGOTH2X",
    "COGOTH3",
    "COGOTH3F",
    "COGOTH3X",
    "CORT",
    "CORTIF",
    NA,
    "CVD",
    "CVDIF",
    NA,
    "DELIR",
    "DELIRIF",
    NA,
    "DEMUN",
    "DEMUNIF",
    NA,
    "DEP",
    "DEPIF",
    NA,
    "DOWNS",
    "DOWNSIF",
    NA,
    "DYSILL",
    "DYSILLIF",
    NA,
    "EPILEP",
    "EPILEPIF",
    NA,
    "ESSTREM",
    "ESSTREIF",
    NA,
    "FTLDMO",
    "FTLDMOIF",
    NA,
    "FTLDNOS",
    "FTLDNOIF",
    NA,
    "HIV",
    "HIVIF",
    NA,
    "HUNT",
    "HUNTIF",
    NA,
    "HYCEPH",
    "HYCEPHIF",
    NA,
    "IMPSUB",
    "IMPSUBIF",
    NA,
    "MEDS",
    "MEDSIF",
    NA,
    "MSA",
    "MSAIF",
    NA,
    "NACCALZD",
    "NACCALZP",
    NA,
    "NACCLBDE",
    "NACCLBDP",
    NA,
    "NEOP",
    "NEOPIF",
    NA,
    "OTHCOG",
    "OTHCOGIF",
    "OTHCOGX",
    "OTHPSY",
    "OTHPSYIF",
    "OTHPSYX",
    "POSSAD",
    "POSSADIF",
    NA,
    "PPAPH",
    "PPAPHIF",
    NA,
    "PRION",
    "PRIONIF",
    NA,
    "PROBAD",
    "PROBADIF",
    NA,
    "PSP",
    "PSPIF",
    NA,
    "PTSDDX",
    "PTSDDXIF",
    NA,
    "SCHIZOP",
    "SCHIZOIF",
    NA,
    "STROKE",
    "STROKIF",
    NA,
    "VASC",
    "VASCIF",
    NA,
    "VASCPS",
    "VASCPSIF",
    NA
  ) |>
    mutate(
      disease = map_chr(presump_etio_diag, \(x) {
        str_split(rdd[[x]]$short_descriptor, " - ", simplify = T)[, 2]
      }) |>
        gsub(x = _, pattern = "\\(specify\\)", replacement = "") |>
        stringr::str_trim()
    )

  usethis::use_data(diag_contr_pairs, overwrite = T)

  nacc_data <- data.table::fread(
    "~/Documents/NACC-data/data/investigator_nacc66.csv"
  )

  nacc_data_prepared <- prepare_data(nacc_data)
}

#' Table With Previous Diagnoses
#'
#' @param dat Data to use. Should only refer to a single participant
#' @param table_font_size Table font size as a percent. Default: 100
prev_diagnoses_table <- function(dat, table_font_size = 100) {
  # due to NSE notes in R CMD check:
  var <-
    val <-
      for_tab <- NULL

  if (!data.table::is.data.table(dat)) {
    cli::cli_abort("The {.var dat} object must be a {.cls data.table}.")
  }

  diagnosis_table <- dat[,
    .SD,
    .SDcol = unlist(data.table::patterns(
      paste(
        "_etiology$",
        "_contribution$",
        "^NACCID$",
        "^VISITDATE$",
        "^NACCUDSD$",
        "^raw_MOCATOTS",
        "^raw_NACCMMSE",
        "^CDR",
        "^FAS",
        sep = "|"
      ),
      cols = colnames(dat)
    ))
  ]

  diagnosis_table[,
    names(.SD) := lapply(.SD, as.character),
    .SDcol = unlist(data.table::patterns(
      "_etiology$|_contribution$",
      cols = colnames(dat)
    ))
  ]

  # diagnosis_table <-

  diagnosis_table <- data.table::melt(
    diagnosis_table,
    measure.vars = unlist(data.table::patterns(
      "_etiology$|_contribution",
      cols = colnames(diagnosis_table)
    ))
  )

  diagnosis_table$nacc_name <- gsub(
    pattern = "_etiology$|_contribution$",
    replacement = "",
    x = diagnosis_table$variable
  )

  diagnosis_table$variable <- gsub(
    pattern = "^[A-Z0-9]+_",
    replacement = "",
    x = diagnosis_table$variable
  )

  diagnosis_table <- data.table::dcast(
    diagnosis_table,
    ... ~ variable,
    value.var = "value"
  )

  diagnosis_table <- diagnosis_table[with(
    diagnosis_table,
    order(NACCID, VISITDATE)
  )]

  diagnosis_table$contribution_character <- c(
    "1" = "Primary",
    "2" = "Contributing",
    "3" = "Non-contributing"
  )[diagnosis_table$contribution]

  diagnosis_table$disease <- with(
    diag_contr_pairs,
    setNames(disease, presump_etio_diag)
  )[
    diagnosis_table$nacc_name
  ]

  diagnosis_table$disease <- with(
    diagnosis_table,
    ifelse(
      grepl(pattern = "^Other", x = disease),
      paste0(disease, ": ", etiology),
      disease
    )
  )

  diagnosis_table$cdr <- with(
    diagnosis_table,
    paste0(CDRGLOB, " (", CDRSUM, ")")
  )

  # fmt: skip
  for (x in intersect(c("raw_MOCATOTS", "raw_NACCMMSE"), colnames(diagnosis_table))) {
    diagnosis_table[[x]] <- NpsychBatteryNorms::valid_values_only(
      raw_score = diagnosis_table[[x]],
      var_name = gsub("^raw_", "", x)
    )
  }

  colnames(diagnosis_table) <- gsub(
    pattern = "^raw_",
    replacement = "",
    x = colnames(diagnosis_table)
  )

  diagnosis_table <- diagnosis_table[,
    list(
      etiologies = list(.SD)
    ),
    by = intersect(
      c(
        "NACCID",
        "VISITDATE",
        "NACCUDSD",
        "MOCATOTS",
        "NACCMMSE",
        "cdr",
        "FAS",
        "contribution_character"
      ),
      colnames(diagnosis_table)
    ),
    .SDcols = "disease"
  ]

  diagnosis_table$etiologies <- lapply(
    diagnosis_table$etiologies,
    `[[`,
    "disease"
  )

  diagnosis_table$etiologies[which(
    is.na(diagnosis_table$NACCUDSD) | diagnosis_table$NACCUDSD == 1
  )] <- list(NA)

  ## Combine MOCATOTS and NACCMMSE
  diagnosis_table$MOCATOTS <- with(
    diagnosis_table,
    ifelse(
      !is.na(MOCATOTS),
      MOCATOTS,
      NACCMMSE
    )
  )

  which_mmse <- diagnosis_table[!is.na(diagnosis_table$NACCMMSE)]$VISITDATE

  diagnosis_table <- diagnosis_table[,
    which(!colnames(diagnosis_table) %in% "NACCMMSE"),
    with = F
  ]

  for_out <- diagnosis_table[
    diagnosis_table$NACCUDSD %in%
      1:4 |
      !is.na(diagnosis_table$contribution_character)
  ]

  if (nrow(for_out) == 0) {
    cli::cli_alert_info("No diagnoses to display.")

    out <- gt::gt(data = data.frame(x = "No previous diagnoses found.")) |>
      gt::cols_label(x = "") |>
      gt::tab_style(
        style = gt::cell_borders(style = "hidden"),
        locations = list(gt::cells_column_labels(), gt::cells_body())
      )

    return(out)
  }

  for_out <- for_out[,
    list(
      "for_tab" = list(
        # data.table::data.table(.SD)
        data.table::setDT(data.table::data.table(
          "var" = factor(
            c(
              "Global Cognition",
              "CDR Global (SOB)",
              "FAS",
              "Diagnosis",
              .SD$contribution_character
            ),
            levels = c(
              "Global Cognition",
              "CDR Global (SOB)",
              "FAS",
              "Diagnosis",
              "Primary",
              "Contributing",
              "Non-contributing"
            )
          ),
          "val" = c(
            unique(.SD$MOCATOTS),
            unique(.SD$cdr),
            unique(.SD$FAS),
            NpsychBatteryNorms::values_to_labels(
              as.numeric(unique(.SD$NACCUDSD)),
              var_name = "NACCUDSD"
            ),
            unname(.SD$etiologies)
          )
        ))[
          ## We're joining on a data.table with all levels of var.
          data.table::CJ(
            "var" = factor(
              c(
                "Global Cognition",
                "CDR Global (SOB)",
                "FAS",
                "Diagnosis",
                "Primary",
                "Contributing",
                "Non-contributing"
              ),
              levels = c(
                "Global Cognition",
                "CDR Global (SOB)",
                "FAS",
                "Diagnosis",
                "Primary",
                "Contributing",
                "Non-contributing"
              )
            ),
            unique = T
          ),
          on = list(var)
        ][,
          list(
            "var" = var,
            "val" = lapply(val, \(x) ifelse(is.null(x), NA, x))
          )
        ]
      )
    ),
    by = c(
      "NACCID",
      "VISITDATE"
    )
  ]

  for_out <- for_out[,
    list(
      "var" = unlist(lapply(for_tab, `[[`, "var"), recursive = F),
      "val" = unlist(lapply(for_tab, `[[`, "val"), recursive = F)
    ),
    by = "VISITDATE"
  ]

  for_out <- for_out[order(for_out$VISITDATE)]

  for_out <- data.table::dcast(
    for_out,
    var ~ VISITDATE,
    value.var = "val"
  )
  for_out <- for_out[order(for_out$var)]

  for_out[,
    c("var", names(.SD)) := c(
      list(factor(
        gsub(pattern = "\\ ", "&nbsp;", var),
        levels = gsub(pattern = "\\ ", "&nbsp;", levels(var))
      )),
      lapply(
        .SD,
        \(x) {
          lapply(x, \(y) {
            if (length(y) == 0 | all(is.na(y))) {
              return(NA)
            }

            paste(
              # stringr::str_replace_all(y, " ", "&nbsp;"),
              gsub(pattern = "\\ ", "&nbsp;", y),
              collapse = '<p style="margin:7px;"></p>'
            )
          })
        }
      )
    ),
    .SDcols = is.list #setdiff("VISITDATE", colnames(for_out))
  ]

  out_gt <- gt::gt(
    for_out,
    id = "diagnosis-table",
    rowname_col = "var"
  ) |>
    gt::fmt_markdown() |>
    gt::sub_missing() |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          style = "italic",
          decorate = "underline"
        )
      ),
      locations = gt::cells_body(
        columns = which(colnames(for_out) %in% which_mmse),
        rows = 1
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          v_align = "top",
          weight = "bold"
        ),
        gt::css(position = "sticky", left = 0)
      ),
      locations = list(
        gt::cells_stub(),
        gt::cells_stubhead()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "right",
        weight = gt::px(2),
        color = "#00000033" # rgb(0,0,0, 0.2)
      ),
      locations = gt::cells_stubhead()
    ) |>
    gt::tab_style(
      style = gt::cell_text(v_align = "top"),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::css(
        "white-space" = "nowrap"
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::cols_align(
      align = "left"
    ) |>
    gt::tab_options(
      column_labels.font.weight = "bold",
      table.font.size = gt::pct(table_font_size)
    ) |>
    gt::opt_css(
      css = "
      #diagnosis-table .gt_table {
        border-collapse: separate;
        border-spacing: 0;
      }

      #diagnosis-table .gt_footnote {
        position: sticky;
        left: 0;
      }
    "
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        weight = "2px",
        color = "#D3D3D3"
      ),
      locations = gt::cells_footnotes()
    ) |>
    gt::tab_footnote(
      footnote = gt::html(paste(
        "MoCA",
        if (length(which_mmse) > 0) "or <u><i>MMSE</u></i>"
      )),
      locations = gt::cells_stub(
        rows = grepl("Cognition", .data$var)
      )
    )

  out_html <- gt::as_raw_html(out_gt)

  ## Make footnote sticky. Need to move the colspan attribute. Also, add line.
  ## First, turn html string into character vector with one line per entry
  out_html <- gsub(
    pattern = "^ +|+$ ",
    replacement = "",
    x = strsplit(as.character(out_html), split = "\n", fixed = T)[[1]]
  )

  ## Next, remove any instance of colspan="1"
  out_html <- gsub(pattern = "colspan=\"1\"", replacement = "", x = out_html)

  ## Now, for all lines with colspan="[0-9]+", move this outside of tag within
  ## its own <td> tag, or (if dealing with sourcenote) remove entirely. If
  ## moved to its own tag, style with border top.
  out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)] <- unlist(
    lapply(
      out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)],
      \(x) {
        ## If this is the source note, simply remove colspan part of the line
        if (grepl(pattern = "gt_sourcenote", x)) {
          return(gsub(pattern = "colspan=\"[0-9]+\"", replacement = T, x))
        }

        ## Extract colspan part
        colspan_match <- gregexpr(pattern = "colspan=\"[0-9]+\"", x)[[1]]
        colspan_expr <- substr(
          x = x,
          start = colspan_match,
          stop = colspan_match + attr(colspan_match, "match.length") - 1
        )

        ## Remove from original line, add after.
        paste0(
          gsub(pattern = colspan_expr, replacement = "", x),
          paste(
            "<td",
            colspan_expr,
            "style=\"border-top-style: solid; border-top-color: #D3D3D3; border-top-width: 2px;\"</td>"
          )
        )
      }
    )
  )

  shiny::HTML(out_html, .noWS = "outside")
}
