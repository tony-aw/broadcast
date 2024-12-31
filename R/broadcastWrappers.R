
#' @keywords Internal
#' @noRd
.C_check_conf_dim <- function(x, y) {
  .Call("C_check_conf_dim", x = as.integer(x), y = as.integer(y))
}

#' @keywords Internal
#' @noRd
.C_pmax <- function(x, y) {
  .Call("C_pmax", x = as.integer(x), y = as.integer(y))
}

