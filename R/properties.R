#' Small property functions
#'
#' @description
#' `ndim()` returns the number of dimensions of an object. \cr
#' `lst.ndim()` returns the number of dimensions of every list-element. \cr
#' `lst.typeof()` returns the (internal) type of every list-element. \cr
#' `len()` is an alias for `length`. \cr
#' `lst.len()` is an alias for `lengths`. \cr
#' \cr
#' These functions were all designed to be efficient. \cr
#' \cr
#' 
#' @param x an object.\cr
#' For functions starting with `lst.`, `x` must be a list (i.e. recursive vector or recursive array). \cr
#'
#' @returns
#' An integer scalar
#'
#'
#' @example inst/examples/bind.R
#' 


#' @name properties
NULL

#' @rdname properties
#' @export
ndim <- function(x) {
  return(length(dim(x)))
}


#' @rdname properties
#' @export
lst.ndim <- function(x) {
  return(.C_lst_ndims(x))
}


#' @rdname properties
#' @export
lst.typeof <- function(x) {
  return(.C_lst_typeof(x))
}
