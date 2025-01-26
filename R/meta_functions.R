

#' @keywords internal
#' @noRd
.mybadge_performance_set2 <- function(x) {
  if(x == "TRUE") x2 <- "TRUE-darkgreen"
  if(x == "FALSE") x2 <- "FALSE-red"
  txt <- paste0("for performance: set to ", x)
  file <- paste0("for_performance-set_to_", x2, ".svg")
  text <- sprintf("\\link[=broadcast_help]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

