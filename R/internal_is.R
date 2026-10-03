

#' @keywords internal
#' @noRd
.is.integer_scalar <- function(x) {
  if(!is.numeric(x) || length(x) != 1) return(FALSE)
  x <- as.integer(x)
  if(is.na(x)) return(FALSE)
  return(TRUE)
}




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
.is_list <- function(x) {
  return(is.list(x) && !is.pairlist(x))
}
