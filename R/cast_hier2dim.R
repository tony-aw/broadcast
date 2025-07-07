#' Cast Hierarchical List into Dimensional list
#'
#' @description
#' `cast_hier2dim()` casts a hierarchical list into a dimensional list
#' (i.e. into a recursive array). \cr
#' `hier2dim()` takes a hierarchical list,
#' and predicts what dimensions the list would have,
#' if re-casted by the `cast_hier2dim()` function. \cr
#' \cr
#' 
#' 
#' @param x a list. \cr
#' If `x` has redundant nesting,
#' it is advisable (though not necessary) to reduce the redundant nesting using \link{dropnests}.
#' @param in2out see \link{broadcast_casting}.
#' @param maxdepth a single, positive integer, giving the maximum depth to drop nesting in. \cr
#' The surface-level elements of a list is depth 1, so `maxdepth` must be `>= 2`.
#' @param recurse_classed `TRUE` or `FALSE`,
#' indicating if the function should also recurse through classed lists within `x`,
#' like data.frames.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' For `cast_hier2dim()`: \cr
#' A recursive array (i.e. a dimensional list). \cr
#' \cr
#' For `hier2dim()`: \cr
#' An integer vector,
#' giving the dimensions `x` would have,
#' if re-casted by `cast_hier2dim()`. \cr
#' \cr
#' 
#'
#' @example inst/examples/cast_hier2dim.R
#' 
#'
#'

#' @rdname cast_hier2dim
#' @export
cast_hier2dim <- function(x, ...) {
  UseMethod("cast_hier2dim", x)
}

#' @rdname cast_hier2dim
#' @export
hier2dim <- function(x, ...) {
  UseMethod("hier2dim", x)
}


#' @rdname cast_hier2dim
#' @export
cast_hier2dim.default <- function(x, in2out = TRUE, maxdepth = 16L, recurse_classed = FALSE, ...) {
  
  out.dims <- hier2dim(x, in2out, maxdepth, recurse_classed)
  out.ndims <- depth <- length(out.dims)
  out.len <- prod(out.dims)
  out.dcp <- c(1, cumprod(out.dims))[1:out.ndims]
  if(in2out) {
    # note that, when in20ut = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  out <- vector("list", out.len)
  dim(out) <- out.dims
  
  .rcpp_rec_cast_hier2dim(x, out, out.dcp, 0, 1.0, depth)
  return(out)
  
}

#' @rdname cast_hier2dim
#' @export
hier2dim.default <- function(x, in2out = TRUE, maxdepth = 16L, recurse_classed = FALSE, ...) {
  
  # check `x`:
  if(!is.list(x)) {
    stop("`x` must be a list")
  }
  if(length(x) == 0L) {
    stop("cannot cast zero-length list")
  }
  if(length(x) > (2^31 - 1)) {
    stop("long vectors not supported")
  }
  if(!is.null(dim(x))) {
    stop("`x` already has dimensions")
  }
  
  maxdepth <- as.integer(maxdepth)
  if(length(maxdepth) != 1L || is.na(maxdepth) || maxdepth < 2L || maxdepth > 16L) {
    stop("`maxdepth` must be a single integer between 2 and 16")
  }
  
  # check binary arguments:
  if(!isTRUE(in2out) && !isFALSE(in2out)) {
    stop("`in2out` must be `TRUE` or `FALSE`")
  }
  if(!isTRUE(recurse_classed) && !isFALSE(recurse_classed)) {
    stop("`recurse_classed` must be `TRUE` or `FALSE`")
  }
  
  # check class & nesting:
  if(!recurse_classed && !is.null(attr(x, "class"))) {
    stop("if `recurse_classed` is `FALSE`, `x` cannot be a classed list")
  }
  if(!.rcpp_hier2dim_surface_OK(x, recurse_classed)) {
    stop("not all surface elements have valid nested elements")
  }
  
  
  # Main Function:
  
  out.ndims <- depth <- .hiercast_depth(x, maxdepth, recurse_classed, sys.call())
  if(depth == 1) {
    stop("not all surface elements have valid nested elements")
  }
  out.dims <- .hiercast_dims(x, depth, in2out, recurse_classed, sys.call())
  
  out.dcp <- c(1, cumprod(out.dims))[1:out.ndims]
  if(in2out) {
    # note that, when in20ut = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  
  return(out.dims)
  
}



