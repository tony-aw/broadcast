

#' @keywords internal
#' @noRd
.is_numeric_like <- function(x) {
  return(is.numeric(x) || is.logical(x))
}


#' @keywords internal
#' @noRd
.is_logical_like <- function(x) {
  return(is.integer(x) || is.logical(x))
}


.is_array_like <- function(x) {
  return(is.array(x) || is.vector(x))
}

#' @keywords internal
#' @noRd
.types <- function() {
  return(c("unknown", "raw", "logical", "integer", "double", "complex", "character", "list"))
}


#' @keywords internal
#' @noRd
.make_int53scalar <- function(x) {
  if(is.na(x) || is.infinite(x)) {
    return(x)
  }
  intmax <- 2^53
  intmin <- -1*intmax
  if(x >= intmin && x <= intmax) {
    return(as_int(x))
  }
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
