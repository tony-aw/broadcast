

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


#' @keywords internal
#' @noRd
.determine_highest_atmoic_type <- function(x, y) {
  if(is.character(x) || is.character(y)) {
    return(5L)
  }
  else if(is.complex(x) || is.complex(y)) {
    return(4L)
  }
  else if(is.double(x) || is.double(y)) {
    return(3L)
  }
  else if(is.integer(x) || is.integer(y)) {
    return(2L)
  }
  else if(is.logical(x) || is.logical(y)) {
    return(1L)
  }
  else {
    return(0L)
  }
}

