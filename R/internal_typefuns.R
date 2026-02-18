

#' @keywords internal
#' @noRd
.is_numeric_like <- function(x) {
  return(is.numeric(x) || is.logical(x))
}


#' @keywords internal
#' @noRd
.is_int32 <- function(x) {
  return(is.integer(x) || is.logical(x))
}

#' @keywords internal
#' @noRd
.is_boolable <- function(x) {
  return(is.integer(x) || is.logical(x) || is.raw(x))
}

#' @keywords internal
#' @noRd
.is_bitable <- function(x) {
  return(is.integer(x) || is.raw(x))
}

#' @keywords internal
#' @noRd
.is_supported_type <- function(x) {
  return(
    is.logical(x) || is.integer(x) || is.double(x) || is.complex(x) || is.character(x) || is.raw(x) || is.list(x)
  )
}


#' @keywords internal
#' @noRd
.is_array_like <- function(x) {
  good_form <- is.array(x) || is.null(dim(x))
  good_S3 <- !isS4(x)
  return(good_form && good_S3)
}


#' @keywords internal
#' @noRd
.is_long_dimless_vector <- function(x) {
  intmax <- 2^31 - 1
  out <- (is.null(dim(x))) && (length(x) >= intmax)
  return(out)
}


#' @keywords internal
#' @noRd
.is_list <- function(x) {
  return(is.list(x) && !is.pairlist(x))
}


#' @keywords internal
#' @noRd
.is_mutatomic <- function(x) {
  good_type <- is.logical(x) || is.integer(x) || is.double(x) || is.complex(x) || is.character(x) || is.raw(x)
  good_form <- !is.null(x) && !is.factor(x)
  good_class <- inherits(x, "mutatomic")
  good_serial <- .rcpp_is_ma(x)
  return(good_type && good_form && good_class && good_serial)
}


#' @keywords internal
#' @noRd
.types <- function() {
  return(c("unknown", "raw", "logical", "integer", "double", "complex", "character", "list", "expression"))
}


#' @keywords internal
#' @noRd
.make_int53scalar <- function(x) {
  if(is.na(x) || is.infinite(x)) {
    return(x)
  }
  intmax <- 2^52 - 1
  intmin <- -1 * intmax
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
