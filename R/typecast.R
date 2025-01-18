#' Atomic and List Type Casting With Names and Dimensions Preserved
#'
#' @description
#' Type casting usually strips away attributes of objects. \cr
#' The functions provided here preserve dimensions, dimnames, and names,
#' which may be more convenient for arrays and array-like objects. \cr
#' \cr
#' The functions are as follows: \cr
#'
#'  * \code{as_bool()}: converts object to atomic type \code{logical} (\code{TRUE, FALSE, NA}).
#'  * \code{as_int()}: converts object to atomic type \code{integer}.
#'  * \code{as_dbl()}: converts object to atomic type \code{double} (AKA numeric).
#'  * \code{as_chr()}: converts object to atomic type \code{character}.
#'  * \code{as_cplx()}: converts object to atomic type \code{complex}.
#'  * \code{as_raw()}: converts object to atomic type \code{raw}.
#'  * \code{as_list()}: converts object to recursive type \code{list}. \cr
#' 
#' `as_num()` is an alias for `as_dbl()`. \cr
#' `as_str()` is an alias for `as_chr()`. \cr
#' \cr
#' See also \link[base]{typeof}. \cr \cr
#' 
#'
#' @param x an R object.
#' @param ... further arguments passed to or from other methods.
#'
#'
#' 
#'
#' @returns
#' The converted object. \cr \cr
#'
#'
#' @example inst/examples/typecast.R
#'
#' 
#'
#'

#' @name typecast
NULL


#' @rdname typecast
#' @export
as_bool <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.logical(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname typecast
#' @export
as_int <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.integer(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname typecast
#' @export
as_dbl <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.double(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname typecast
#' @export
as_num <- as_dbl


#' @rdname typecast
#' @export
as_chr <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.character(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname typecast
#' @export
as_str <- as_chr



#' @rdname typecast
#' @export
as_cplx <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.complex(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname typecast
#' @export
as_raw <- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.raw(x, ...)
  attributes(out) <- temp.attr
  return(out)
}


#' @rdname typecast
#' @export
as_list<- function(x, ...) {
  temp.attr <- .attr_typecast(x)
  out <- as.list(x, ...)
  attributes(out) <- temp.attr
  return(out)
}




#' @keywords internal
#' @noRd
.attr_typecast <- function(x) {
  temp.attr <- attributes(x)[c("dim", "dimnames", "names")]
  return(temp.attr)
}
