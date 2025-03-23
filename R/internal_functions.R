#


#' @keywords internal
#' @noRd
.n_args <- function(f) {
  n.args <- names(formals(args(f)))
  n.args <- setdiff(n.args, "...") |> length()
  return(n.args)
}


#' @keywords internal
#' @noRd
.transform_function <- function(f) {
  
  # this function is used for `bcapply()`
  # by this transformation,
  # I don't have to write C code for every single possible combination of
  # typeof(x) and typeof(y)
  
  fnew <- function(x, y, flatind_x, flatind_y) {
    x <- x[[flatind_x]]
    y <- y[[flatind_y]]
    return(f(x, y))
  }
  return(fnew)
}


#' @keywords internal
#' @noRd
.make_dcp <- function(dims) {
  return(c(1, cumprod(dims)))
}


#' @keywords internal
#' @noRd
.return_missing <- function(x) {
  if(is.logical(x)) {
    return(rep(NA, length(x)))
  }
  else if(is.integer(x)) {
    return(rep(NA_integer_, length(x)))
  }
  else if(is.double(x)) {
    return(rep(NA_real_, length(x)))
  }
  else if(is.complex(x)) {
    return(rep(NA_complex_, length(x)))
  }
  else if(is.character(x)) {
    return(rep(NA_character_, length(x)))
  }
  else if(is.list(x)) {
    return(rep(list(NULL), length(x)))
  }
}



#' @keywords internal
#' @noRd
.is.even <- function(x) {
  return(round(x/2) == x/2)
}

#' @keywords internal
#' @noRd
.is.integer_scalar <- function(x) {
  if(!is.numeric(x) || length(x) != 1) return(FALSE)
  x <- as.integer(x)
  if(is.na(x)) return(FALSE)
  return(TRUE)
}
