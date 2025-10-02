#' Internal functions
#'
#'
#'
#'
#'
#'


#' @keywords internal
#' @noRd
.mybadge_casting <- function(x, y, color) {
  filepath <- paste0(gsub(" ", "", x), "-",
                     y, "-", color, ".svg")
  text <- sprintf("\\link[=broadcast_casting]{%s}: %s; ", x, y)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    filepath, toupper(y))
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.recurse_classed <- function(ellipsis, abortcall) {
  if("recurse_classed" %in% names(ellipsis)) {
    txt <- paste0(
      "`recurse_classed` has been replaced with `recurse_all`; \n",
      "please use `recurse_all` instead"
    )
    stop(simpleError(txt, call = abortcall))
  }
  
  if(length(ellipsis)) {
    stop(simpleError("unkown arguments given", call = abortcall))
  }
  
}

  