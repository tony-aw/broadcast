#' Get the Number of Dimensions of an Array
#'
#' @description
#' `ndim()` returns the number of dimensions of an object. \cr
#' `lst.ndim()` returns the number of dimensions of every list-element. \cr
#' `undim()` returns a copy of an object, but with its dimensions removed
#' (it somewhat like the dimensional version of `unlist()`). \cr
#' \cr
#' 
#' @param x a vector or array (for `ndim()`), or a list of vectors/arrays (for `lst.ndim()`). \cr
#'
#' @returns
#' For `ndim()`: an integer scalar. \cr
#' For `lst.ndim()`: an integer vector, with the same length, names and dimensions as `x`. \cr
#' For `undim()`: the original object, but without dimensions. \cr \cr
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
  broadcaster(out) <- broadcaster(x)
  return(out)
}

#' @rdname ndim
#' @export
undim <- function(x) {
  dim(x) <- NULL
  return(x)
}
