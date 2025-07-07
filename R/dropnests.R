#' Drop Redundant Nesting
#'
#' @description
#' `dropnests()` drops redundant nesting of a list. \cr
#' It is the hierarchical equivalent to the dimensional \link[base]{drop} function. \cr
#' \cr
#' 
#' 
#' @param x a list
#' @param maxdepth a single, positive integer, giving the maximum depth to drop nesting in. \cr
#' The surface-level elements of a list is depth 1;
#' so when `maxdepth = 1`, `x` will be returned unchanged.
#' @param recurse_classed `TRUE` or `FALSE`,
#' indicating if the function should also recurse through classed lists within `x`,
#' like data.frames.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' A flattened list,
#' with names that indicate the original dimensional positions of the elements. \cr
#' \cr
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
dropnests.default <- function(x, maxdepth = 16L, recurse_classed = FALSE, ...) {
  stopifnot(is.list(x))
  
  maxdepth <- as.integer(maxdepth)
  if(length(maxdepth) != 1L || is.na(maxdepth) || maxdepth < 1L) {
    stop("`maxdepth` must be a single integer >= 1")
  }
  if(!isTRUE(recurse_classed) && !isFALSE(recurse_classed)) {
    stop("`recurse_classed` must be `TRUE` or `FALSE`")
  }
  if(!recurse_classed && !is.null(attr(x, "class"))) {
    stop("if `recurse_classed` is `FALSE`, `x` cannot be a classed list")
  }
  if(maxdepth == 1L) {
    return(x)
  }
  
  out <- .rcpp_dropnests(x, maxdepth, recurse_classed)
  mostattributes(out) <- attributes(x)
  return(out)
}
