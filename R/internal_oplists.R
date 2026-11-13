
# general ====

#' @keywords internal
#' @noRd
.op_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}


# decimal numeric ====

#' @keywords internal
#' @noRd
.op_dec_math <- function() {
  return(c("+", "-", "*", "/", "^"))
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
.op_dec_pminmax <- function() {
  return(c("pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_dec_boolint_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">=", "d==", "d!=", "d<", "d>", "d<=", "d>="))
}


# integer specific ====

#' @keywords internal
#' @noRd
.op_int_main <- function() {
  return(c("gcd", "%%", "%/%", "pmin", "pmax"))
}

#' @keywords internal
#' @noRd
.op_int_rel <- function() {
  return(c("==", "!=", "<", ">", "<=", ">="))
}

.op_int_boolint_math <- function() {
  return(c("pmin", "pmax", "gcd", "%%", "%/%"))
}


# logical/boolean ====

#' @keywords internal
#' @noRd
.op_b <- function() {
  return(c("&", "|", "xor", "nand", "nor", "==", "!="))
}


# string ====

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

# complex numeric ====

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


# bit-wise ====

#' @keywords internal
#' @noRd
.op_bit <- function() {
  return(c("&", "|", "xor", "nand", "nor", "<<", ">>"))
}


# raw bytes ====

#' @keywords internal
#' @noRd
.op_raw_byte <- function() {
  return(c("==", "!=", "<", ">", "<=", ">=", "pmin", "pmax", "diff"))
}
