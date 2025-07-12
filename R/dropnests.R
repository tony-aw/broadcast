#' Drop Redundant List Nesting
#'
#' @description
#' `dropnests()` drops redundant nesting of a list. \cr
#' It is the hierarchical equivalent to the dimensional \link[base]{drop} function. \cr
#' \cr
#' `depth_range()` gives the minimum and maximum depth of a list. \cr
#' 
#' 
#' @param x a list
#' @param maxdepth a single, positive integer,
#' giving the maximum depth to recurse into the list. \cr
#' The surface-level elements of a list is depth 1. \cr
#' `dropnests(x, maxdepth = 1)` will return `x` unchanged.
#' @param recurse_classed `TRUE` or `FALSE`,
#' indicating if the function should also recurse through classed lists within `x`,
#' like data.frames.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' For `dropnests()`: \cr
#' A list without redundant nesting. \cr
#' Attributes are preserved. \cr
#' \cr
#' For `depth_range()`: \cr
#' An integer vector of length 2,
#' where the first element gives the minimum depth found,
#' and the second element gives the maximum depth found. \cr \cr
#' 
#'
#' @example inst/examples/dropnests.R
#' 
#'
#'

#' @rdname dropnests
#' @export
dropnests <- function(x, ...) {
  UseMethod("dropnests", x)
}

#' @rdname dropnests
#' @export
depth_range <- function(x, ...) {
  UseMethod("depth_range", x)
}


#' @rdname dropnests
#' @export
dropnests.default <- function(x, maxdepth = 32L, recurse_classed = FALSE, ...) {
  .depth_check(x, maxdepth, recurse_classed, sys.call())
  
  if(maxdepth == 1L) {
    return(x)
  }
  
  out <- .rcpp_dropnests(x, maxdepth, recurse_classed)
  mostattributes(out) <- attributes(x)
  return(out)
}


#' @rdname dropnests
#' @export
depth_range.default <- function(x, maxdepth = 32L, recurse_classed = FALSE, ...) {
  .depth_check(x, maxdepth, recurse_classed, sys.call())
  
  out <- .rcpp_depth_range(x, maxdepth, recurse_classed)
  return(out)
}


#' @keywords internal
#' @noRd
.depth_check <- function(x, maxdepth, recurse_classed, abortcall) {
  
  if(!is.list(x)) {
    stop(simpleError("`x` must be a list", call = abortcall))
  }
  maxdepth <- as.integer(maxdepth)
  if(length(maxdepth) != 1L || is.na(maxdepth) || maxdepth < 1L) {
    stop(simpleError("`maxdepth` must be a single integer >= 1",
                     call = abortcall))
  }
  if(!isTRUE(recurse_classed) && !isFALSE(recurse_classed)) {
    stop(simpleError("`recurse_classed` must be `TRUE` or `FALSE`",
                     call = abortcall))
  }
  if(!recurse_classed && !is.null(attr(x, "class"))) {
    stop(simpleError(
      "if `recurse_classed` is `FALSE`, `x` cannot be a classed list",
      call = abortcall
    ))
  }
}

