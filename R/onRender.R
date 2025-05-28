#' See `help(htmlwidgets::onRender)`
#'
#' @param x HTML Widget object
#' @param jsCode Character vector containing JavaScript code
#' @param data Additional argument passed to jsCode. Can be any R object that can be serialized to JSON.
#'
#' @export
onRender <- function(x, jsCode, data = NULL) {
  if (length(jsCode) == 0) return(x)
  if (length(jsCode) > 1) jsCode <- paste(jsCode, collapse = "\n")
  x$jsHooks[["render"]] <- c(
    x$jsHooks[["render"]],
    list(list(code = jsCode, data = data))
  )
  x
}
