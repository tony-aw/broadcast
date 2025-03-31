#' Get number of dimensions
#'
#' @description
#' `ndim()` returns the number of dimensions of an object. \cr
#' `lst.ndim()` returns the number of dimensions of every list-element. \cr
#' \cr
#' 
#' @param x an object.\cr
#' For functions starting with `lst.`, `x` must be a list (i.e. recursive vector or recursive array). \cr
#'
#' @returns
#' For `ndim()`: an integer scalar. \cr
#' For `lst.ndim()`: an integer vector, with the same length, names and dimensions as `x`. \cr
#'
#'
#' @example inst/examples/ndim.R
#' 


#' @name ndim
NULL

#' @rdname ndim
#' @export
ndim <- function(x) {
  return(length(dim(x)))
}


#' @rdname ndim
#' @export
lst.ndim <- function(x) {
  out <- .C_lst_ndims(x)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}
