

#' @keywords internal
#' @noRd
.types <- function() {
  return(c("unknown", "raw", "logical", "integer", "double", "complex", "character", "list", "expression"))
}


#' @keywords internal
#' @noRd
.trymake_simple_int_scalar <- function(x) {
  
  if(is.nan(x) || is.infinite(x)) return(x)
  if(is.na(x)) return(as_lgl(x))
  if(x == 1L || x == 0L) return(as_lgl(x))
  
  intmax <- 2^31 - 1
  intmin <- -1 * intmax
  if(x >= intmin && x <= intmax && round(x) == x) return(as_int(x))
  
  return(x)
}


#' @keywords internal
#' @noRd
.type_alias_coerce <- function(out.type, abortcall) {
  if(out.type == "list") {
    coerce <- as.list
  }
  else if(out.type == "character") {
    coerce <- as.character
  }
  else if(out.type == "complex") {
    coerce <- as.complex
  }
  else if(out.type == "double") {
    coerce <- as.double
  }
  else if(out.type == "integer") {
    coerce <- as.integer
  }
  else if(out.type == "logical") {
    coerce <- as.logical
  }
  else if(out.type == "raw") {
    coerce <- as.raw
  }
  else {
    stop(simpleError("unknown type", call = abortcall))
  }
  return(coerce)
}

#' @keywords internal
#' @noRd
.type_alias_coerce2 <- function(out.type, abortcall) {
  if(out.type == "list") {
    coerce <- as_list
  }
  else if(out.type == "character") {
    coerce <- as_chr
  }
  else if(out.type == "complex") {
    coerce <- as_cplx
  }
  else if(out.type == "double") {
    coerce <- as_dbl
  }
  else if(out.type == "integer") {
    coerce <- as_int
  }
  else if(out.type == "logical") {
    coerce <- as_bool
  }
  else if(out.type == "raw") {
    coerce <- as_raw
  }
  else {
    stop(simpleError("unknown type", call = abortcall))
  }
  return(coerce)
}
