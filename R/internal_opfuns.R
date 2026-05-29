
#' @keywords internal
#' @noRd
.op_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}

#' @keywords internal
#' @noRd
.op_dec_math <- function() {
  return(c("+", "-", "*", "/", "^", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_dec_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}

#' @keywords internal
#' @noRd
.op_dec_dist <- function() {
  return(c("d==", "d!=", "d<", "d>", "d<=", "d>="))
}


#' @keywords internal
#' @noRd
.op_int_math <- function() {
  return(c("+", "-", "*", "^", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_int_d <- function() {
  return(c("gcd", "%%", "%/%"))
}

#' @keywords internal
#' @noRd
.op_int_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}


#' @keywords internal
#' @noRd
.op_b <- function() {
  return(c("&", "|", "xor", "nand", "nor"))
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
  return(c("levenshtein", "lcss"))
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


#' @keywords internal
#' @noRd
.op_bit <- function() {
  return(c("&", "|", "xor", "nand", "nor", "<<", ">>"))
}


#' @keywords internal
#' @noRd
.op_raw_byte <- function() {
  return(c("==", "!=", "<", ">", "<=", ">=", "pmin", "pmax", "diff"))
}
