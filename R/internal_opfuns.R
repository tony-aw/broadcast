
#' @keywords internal
#' @noRd
.op_num_math <- function() {
  return(c("+", "-", "*", "/", "^", "intmod", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_num_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">=", "d==", "d!=", "d<", "d>", "d<=", "d>="))
}

#' @keywords internal
#' @noRd
.op_b_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}

#' @keywords internal
#' @noRd
.op_b_andor <- function() {
  return(c("&", "|", "xor", "nand"))
}

#' @keywords internal
#' @noRd
.op_str_rel <- function() {
  return(c("==", "!="))
}

#' @keywords internal
#' @noRd
.op_str_conc <- function() {
  return(c("+"))
}

