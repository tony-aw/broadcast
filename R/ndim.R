#' Get the Number of Dimensions of an Array
#'
#' @description
#' `ndim()` returns the number of dimensions of an object. \cr
#' `lst.ndim()` returns the number of dimensions of every list-element. \cr
#' \cr
#' 
#' @param x a vector or array (for `ndim()`), or a list of vectors/arrays (for `lst.ndim()`). \cr
#'
#' @returns
#' For `ndim()`: an integer scalar. \cr
#' For `lst.ndim()`: an integer vector, with the same length, names and dimensions as `x`. \cr \cr
#'
#'
#' @example inst/examples/ndim.R
#' 


#' @name ndim
NULL

#' @rdname ndim
#' @export
ndim <- function(x) {
  return(.rcpp_clone(length(dim(x))))
}


#' @rdname ndim
#' @export
lst.ndim <- function(x) {
  out <- .rcpp_lst_ndims(x)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  broadcaster(out) <- broadcaster(x)
  return(out)
}
