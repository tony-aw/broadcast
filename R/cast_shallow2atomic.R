#' Cast Shallow List to Atomic Object
#'
#' @description
#' `cast_shallow2atomic()` casts a shallow (i.e. non-nested) list to an atomic object. \cr
#' \cr
#' 
#' 
#' @param x a shallow (i.e. non-nested) list. \cr
#' The attributes of the objects inside the list will be ignored, except for `names`.
#' @param arrangement see the `Details` and `Examples` sections.
#' @param comnames_from an integer scalar or `NULL`, and only relevant if `arrangement` is 1 or -1. \cr
#' This gives which element of `x` to use for the communal names. \cr
#' If `NULL`, no communal names will be given. \cr
#' For example: \cr
#' If `x` is a 1d (or dimensionless) list,
#' `cast_shallow2atomic(x, 1)` will produce an atomic matrix. \cr
#' The column names of the matrix will be `names(x)`. \cr
#' The row names, however, will be taken from `names(x[[comnames_from]])`,
#' provided that `x[[comnames_from]]` has the proper length. \cr
#' See also the `Examples` section.
#' @param padding an atomic scalar, and only relevant if `arrangement` is 1 or -1. \cr
#' This gives the padding value to use when padding is required. \cr
#' Padding is used to ensure every all slices of the same dimension in the output
#' have equal number of elements
#' (for example, all rows must have the same number of columns).
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @details
#' If `arrangement = 0L`, \cr
#' `cast_shallow2atomic()` works like `unlist()`,
#' except that `cast_shallow2atomic()` guarantees an atomic vector result. \cr
#' \cr
#' If `arrangement = 1L`, \cr
#' `cast_shallow2atomic()` will produce an atomic array,
#' with the elements arranged such that the dimensions are `c(max(lengths(x)), dim(x))`. \cr
#' If `x` has no dimensions, `dim(x)` is replaced with `length(x)`, thus treating `x` as an 1d array. \cr
#' This will therefore always produce an atomic array with at least 2 dimensions. \cr
#' \cr
#' If `arrangement = -1L`, \cr
#' `cast_shallow2atomic()` will produce an atomic array,
#' with the elements arranged such that the dimensions are `c(dim(x), max(lengths(x)))`. \cr
#' If `x` has no dimensions, `dim(x)` is replaced with `length(x)`, thus treating `x` as an 1d array. \cr
#' This will therefore always produce an atomic array with at least 2 dimensions. \cr
#' \cr
#' 
#' 
#' @section Back transformation:
#' From the casted atomic object, \cr
#' `out <- cast_shallow2atomic(x, ...)`, \cr
#' one can get an \emph{approximation} of the original shallow list back using just base 'R' functions. \cr
#' This section describes how to do so. \cr
#' \cr
#' \bold{`arrangement = 0L`} \cr
#' If `arrangement = 0L`, one can transform an atomic object `out` back to a shallow list using: \cr
#' `back <- as.list(out)` \cr
#' `names(back) <- names(out)` \cr
#' \cr
#' 
#' \bold{`arrangement = 1L`} \cr
#' If `arrangement = 1L`, one can transform an atomic object `out` back to a shallow list using: \cr
#' `asplit(out, seq(2, ndim(out)))` \cr
#' \cr
#' 
#' \bold{`arrangement = -1L`} \cr
#' If `arrangement = -1L`, one can transform an atomic object `out` back to a shallow list using: \cr
#' `asplit(out, seq(1, ndim(out) - 1L))` \cr
#' \cr
#' \cr
#' 
#' @returns
#' If `arrangement = 0L`: \cr
#' An atomic vector without dimensions. \cr
#' \cr
#' If `arrangement = 1L`: \cr
#' An atomic array with dimensions `c(max(lengths(x)), dim(x))`. \cr
#' The dimnames, if possible to construct, will be `c(names(x[[comnames_from]]), dimnames(x))`, provided 
#' If `x` has no dimensions, `dim(x)` is replaced with `length(x)`. \cr
#' \cr
#' If `arrangement = -1L`: \cr
#' An atomic array with dimensions `c(dim(x), max(lengths(x)))`. \cr
#' If `x` has no dimensions, `dim(x)` is replaced with `length(x)`. \cr
#' \cr
#' 
#' 
#' @seealso \link{broadcast_casting} \cr
#' 
#' @example inst/examples/cast_shallow2atomic.R
#' 
#'
#'

#' @rdname cast_shallow2atomic
#' @export
cast_shallow2atomic <- function(x, ...) {
  UseMethod("cast_shallow2atomic", x)
}


#' @rdname cast_shallow2atomic
#' @export
cast_shallow2atomic.default <- function(x, arrangement = 0L, padding = NA, comnames_from = 1L, ...) {
  
  .ellipsis(list(...), sys.call())
  .unlist_check(x, arrangement, padding, comnames_from, sys.call())
  maxlen <- .C_unlisthelper_maxlen(x)
  if(maxlen == 0L) {
    stop("all elements of `x` have length zero")
  }
  
  # output type:
  out.type <- .rcpp_unlisthelper_max_type(x)
  out.type <- .types()[out.type]
  if(out.type == "unknown") {
    stop("list contains unknown types")
  }
  if(out.type == "expression") {
    warning("expression-like elements found; coercing to character")
    out.type <- "character"
  }
  mycoerce <- .type_alias_coerce2(out.type, sys.call())
  
  if(arrangement == 0L) {
    x <- lapply(x, mycoerce)
    return(unlist(x, use.names = TRUE, recursive = FALSE))
  }
  else {
    return(.unlist_dim(x, maxlen, arrangement, mycoerce, padding, comnames_from))
  }
  
}





#' @keywords internal
#' @noRd
.unlist_check <- function(x, arrangement, padding, comnames_from, abortcall) {
  
  # check `x`:
  if(!.is_list(x)) {
    stop(simpleError("`x` must be a shallow list", call = abortcall))
  }
  if(length(x) == 0L) {
    stop(simpleError("cannot cast zero-length list", call = abortcall))
  }
  if(length(x) > (2^31 - 1)) {
    stop(simpleError("long vectors not supported", call = abortcall))
  }
  
  # check other:
  if(!is.numeric(arrangement) || length(arrangement) != 1 || is.na(arrangement) || !arrangement %in% c(-1L:1L)) {
    stop(simpleError("`arrangement` must be 0, 1 or -1", call = abortcall))
  }
  if(!is.atomic(padding) || length(padding) != 1L) {
    stop(simpleError("`padding` must be an atomic scalar", call = abortcall))
  }
  if(!is.null(comnames_from)) {
    if(!.is.integer_scalar(comnames_from) || comnames_from < 1L || comnames_from > length(x)){
      stop(simpleError(
        "`comnames_from` must be either `NULL` or a scalar integer >= 1 and <= length(x)",
        call = abortcall
      ))
    }
  }
  
  
}


#' @keywords internal
#' @noRd
.unlist_dim <- function(x, maxlen, arrangement, mycoerce, padding, comnames_from) {
  
  # get params:
  padding <- mycoerce(padding)
  maxint <- 2^31 - 1L
  x.dim <- dim(x)
  if(is.null(x.dim)) {
    x.dim <- length(x)
  }
  if(any(x.dim) >= maxint ) {
    stop("result would exceed maximum dimension size")
  }
  out.dimnames <- .unlist_getnames(x, x.dim, maxlen, arrangement, comnames_from)
  
  
  # make output dimensions:
  if(arrangement == 1L) {
    nrow <- maxlen
    ncol <- prod(x.dim)
    out.dim <- c(maxlen, x.dim)
  }
  if(arrangement == -1L) {
    nrow <- prod(x.dim)
    ncol <- maxlen
    out.dim <- c(x.dim, maxlen)
  }
  
  # make output:
  x <- lapply(x, mycoerce)
  out <- array(padding, out.dim)
  .set_dimnames(out, out.dimnames)
  .rcpp_shallow2atomic(x, out, nrow, ncol, arrangement)
  return(out)
}

#' @keywords internal
#' @noRd
.unlist_getnames <- function(x, x.dim, maxlen, arrangement, comnames) {
  x.dimnames <- NULL
  
  if(length(x.dim) <= 1L) {
    if(!is.null(names(x))) {
      x.dimnames <- list(names(x))
    }
  }
  else {
    x.dimnames <- dimnames(x)
  }
  
  
  if(!is.null(comnames)) {
    if(length(x[[comnames]]) == maxlen) {
      comnames <- names(x[[comnames]])
    }
    else {
      comnames <- NULL
    }
  }
  
  
  if(is.null(x.dimnames) && is.null(comnames)) {
    return(NULL)
  }
  
  if(is.null(x.dimnames)) {
    x.dimnames <- rep(list(NULL), length(x.dim))
  }
  if(arrangement == 1) {
    out.dimnames <- c(list(comnames), x.dimnames)
  }
  if(arrangement == -1) {
    out.dimnames <- c(x.dimnames, list(comnames))
  }
  
  return(out.dimnames)
  
}


