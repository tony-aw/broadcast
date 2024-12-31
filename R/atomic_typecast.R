#' Atomic Type Casting With Names and Dimensions Preserved
#'
#' @description
#' Atomic type casting in R is generally performed using the functions
#' \link[base]{as.logical}, \link[base]{as.integer},
#' \link[base]{as.double}, \link[base]{as.character},
#' \link[base]{as.complex}, and \link[base]{as.raw}. \cr
#' \cr
#' Converting an object between atomic types using these functions
#' strips the object of its attributes,
#' including (dim)names and dimensions. \cr
#' \cr
#' The functions provided here by the 'tinycodet' package
#' preserve the dimensions, dimnames, and names. \cr
#' \cr
#' The functions are as follows: \cr
#'
#'  * \code{as_bool()}: converts object to atomic type \code{logical} (\code{TRUE, FALSE, NA}).
#'  * \code{as_int()}: converts object to atomic type \code{integer}.
#'  * \code{as_dbl()}: converts object to atomic type \code{double} (AKA decimal numbers).
#'  * \code{as_chr()}: converts object to atomic type \code{character}.
#'  * \code{as_cplx()}: converts object to atomic type \code{complex}.
#'  * \code{as_raw()}:converts object to atomic type \code{raw}.
#'
#'
#' @param x vector, matrix, array
#' (or a similar object where all elements share the same type).
#' @param ... further arguments passed to or from other methods.
#'
#'
#' 
#'
#' @returns
#' The converted object. \cr \cr
#'
#'
#' @example inst/examples/atomic_typecast.R
#'
#' 
#'
#'

#' @name atomic_typecast
NULL


#' @rdname atomic_typecast
#' @export
as_bool <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.logical(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname atomic_typecast
#' @export
as_int <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.integer(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname atomic_typecast
#' @export
as_dbl <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.double(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname atomic_typecast
#' @export
as_chr <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.character(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname atomic_typecast
#' @export
as_cplx <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.complex(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname atomic_typecast
#' @export
as_raw <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.raw(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @keywords internal
#' @noRd
.attr_typecast <- function(x) {
  temp.attr <- attributes(x)[c("dim", "dimnames", "names")]
  return(temp.attr)
}
