
#' @keywords internal
#' @noRd
.op_dec_math <- function() {
  return(c("+", "-", "*", "/", "^", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_dec_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">=", "d==", "d!=", "d<", "d>", "d<=", "d>="))
}

#' @keywords internal
#' @noRd
.op_int_math <- function() {
  return(c("+", "-", "*", "/", "^", "%%", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_int_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}

#' @keywords internal
#' @noRd
.op_int_special <- function() {
  return(c("gcd", "lcm"))
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


#' @keywords internal
#' @noRd
.op_str_dist <- function() {
  return(c("levenshtein"))
}

#' @keywords internal
#' @noRd
.op_cplx_rel <- function() {
  return(c("==", "!="))
}

#' @keywords internal
#' @noRd
.op_cplx_math <- function() {
  return(c("+", "-", "*", "/"))
}
