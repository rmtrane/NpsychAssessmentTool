#' Read data
#'
#' @description
#' Function used to read in data based on the output returned from `dataSelectServer()`.
#'
#' @param data_source One of `"redcap"`, `"csv_upload"`, or `"demo"`.
#' @param data_type Optional. If `data_source` is `"redcap"`, then must be one of `"wadrc_uds2"`, `"wadrc_uds3"` or `"wadrc_uds4"`.
#' @param redcap_auth Optional. A list with entries `redcap_uri` and `token` (when `data_source = "redcap"`).
#' @param data_file Optional. A character vector with path to data file (when `data_source = "csv"`).
#'
#' @returns
#' A `data.table` object.
#'
#' @export
read_data <- function(
  data_source = "redcap",
  data_type = NULL, # c("wadrc_uds3", "wadrc_uds4"),
  redcap_auth, # list with entries redcap_uri and token (when data_source = "redcap")
  data_file # character vector with path to data file (when data_source = "csv")
) {
  if (data_source == "demo") {
    if (!is.null(data_type)) {
      cli::cli_warn(
        "{.arg data_type} is ignored when {.arg data_source} is {.val demo}"
      )
    }

    return(demo_data)
  }

  if (data_source == "redcap") {
    if (!missingArg(data_file) && !is.null(data_file)) {
      cli::cli_warn(
        "{.arg data_file} is ignored when {.arg data_source} is {.val redcap}."
      )
    }
    if (missingArg(redcap_auth)) {
      cli::cli_abort(
        "{.arg redcap_auth} must be provided when {.arg data_source} is {.val redcap}"
      )
    } else {
      if (!inherits(redcap_auth, "list")) {
        cli::cli_abort(
          "{.arg redcap_auth} must be a {.cls list}, but is {.cls {class(redcap_auth)}}."
        )
      }

      if (!all(c("redcap_uri", "token") %in% names(redcap_auth))) {
        cli::cli_abort(
          "{.arg redcap_auth} must have entries named {.val redcap_uri} and {.val token}."
        )
      }

      if (is.null(redcap_auth$redcap_uri) | is.null(redcap_auth$token)) {
        cli::cli_abort(
          "{.arg redcap_auth$redcap_uri} and {.arg redcap_auth$token} must be strings, not NULL."
        )
      }
    }

    if (
      !is.null(data_type) &&
        !data_type %in% c("wadrc_uds2", "wadrc_uds3", "wadrc_uds4")
    ) {
      cli::cli_abort(
        "{.arg data_type} must be {.val wadrc_uds3} or {.val wadrc_uds4} 
        when {.arg data_source} is {.val redcap}."
      )
    }

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "redcap_url_api_found")
      shiny::showNotification(
        "Pulling data from REDCap...",
        duration = NULL,
        type = "message",
        id = "pulling_from_redcap"
      )
    }

    redcap_fields <- switch(
      data_type,
      "wadrc_uds2" = wadrc_uds2_redcap_fields,
      "wadrc_uds3" = wadrc_uds3_redcap_fields,
      "wadrc_uds4" = wadrc_uds4_redcap_fields
    )

    suppressMessages(
      from_redcap <- try(
        REDCapR::redcap_read_oneshot(
          redcap_uri = redcap_auth$redcap_uri,
          token = redcap_auth$token,
          fields = redcap_fields,
          guess_max = Inf
        ),
        silent = TRUE
      )
    )

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "pulling_from_redcap")
    }

    if (inherits(from_redcap, "try-error")) {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          "Unable to access REDCap",
          type = "error"
        )
        return()
      } else {
        cli::cli_abort(
          message = c(
            "Unable to access REDCap (1). {.fn REDCapR::redcap_read_oneshot} returned the error:",
            as.character(from_redcap)
          )
        )
      }
    }

    if (from_redcap$success) {
      from_redcap <- data.table::data.table(from_redcap$data)
    } else {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          "Unable to access REDCap",
          type = "error"
        )
        return()
      } else {
        cli::cli_abort(
          message = c(
            "Unable to access REDCap (2). {.fn REDCapR::redcap_read_oneshot} returned the error:",
            from_redcap$outcome_message
          )
        )
      }
    }

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::showNotification(
        "Preparing REDCap data...",
        duration = NULL,
        type = "message",
        id = "preparing_from_redcap"
      )
    }

    from_redcap <- wadrc_data_prep(
      adrc_data = from_redcap,
      uds = switch(
        data_type,
        "wadrc_uds2" = "uds2",
        "wadrc_uds3" = "uds3",
        "wadrc_uds4" = "uds4"
      )
    )

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "preparing_from_redcap")
      shiny::showNotification(
        "REDCap data successfully pulled and prepared!",
        duration = 3,
        type = "message"
      )
    }

    return(from_redcap)
  }

  if (data_source == "csv_upload") {
    from_csv <- data.table::fread(
      file = data_file,
      na.strings = c("", "NA")
    )

    if (
      !is.null(data_type) &&
        data_type %in% c("wadrc_uds2", "wadrc_uds3", "wadrc_uds4")
    ) {
      from_csv <- wadrc_data_prep(
        adrc_data = from_csv,
        uds = switch(
          data_type,
          "wadrc_uds2" = "uds2",
          "wadrc_uds3" = "uds3",
          "wadrc_uds4" = "uds4"
        )
      )
    }

    return(from_csv)
  }
}
