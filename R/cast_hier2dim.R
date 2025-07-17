#' Cast Hierarchical List into Dimensional list
#'
#' @description
#' `cast_hier2dim()` casts a hierarchical/nested list into a dimensional list
#' (i.e. an array of type `list`). \cr
#' `hier2dim()` takes a hierarchical/nested list,
#' and predicts what dimensions the list would have,
#' if casted by the `cast_hier2dim()` function. \cr
#' \cr
#' 
#' 
#' @param x a nested list. \cr
#' If `x` has redundant nesting,
#' it is advisable (though not necessary) to reduce the redundant nesting using \link{dropnests}.
#' @param in2out see \link{broadcast_casting}.
#' @param maxdepth a single, positive integer,
#' giving the maximum depth to recurse into the list. \cr
#' The surface-level elements of a list is depth 1. \cr
#' @param recurse_classed `TRUE` or `FALSE`,
#' indicating if the function should also recurse through classed lists within `x`,
#' like data.frames.
#' @param padding a list of length `1`,
#' giving the padding value to use when padding is required.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' For `hier2dim()`: \cr
#' An integer vector,
#' giving the dimensions `x` would have,
#' if casted by `cast_hier2dim()`. \cr
#' The names of the output indicates if padding is required (name "padding"),
#' or no padding is required (no name) for that dimension; \cr
#' Padding will be required if not all list-elements at a certain depth have the same length. \cr
#' \cr
#' For `cast_hier2dim()`: \cr
#' An array of type `list`, with the dimensions given by `hier2dim()`. \cr
#' If the output needs padding (indicated by `hier2dim()`),
#' the output will have more elements than `x`,
#' filled with `NULL` as padding,
#' to ensure every all slices of the same dimension have equal number of elements
#' (for example, all rows must have the same number of columns). \cr
#' \cr
#' 
#' 
#'
#' @seealso \link{broadcast_casting} \cr
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
cast_hier2dim.default <- function(x, in2out = TRUE, maxdepth = 16L, recurse_classed = FALSE, padding = list(NULL), ...) {
  
  if(!is.list(padding) || length(padding) > 1L) {
    stop("`padding` must be a list of length 1")
  }
  
  out.dims <- hier2dim(x, in2out, maxdepth, recurse_classed)
  out.ndims <- depth <- length(out.dims)
  out.len <- prod(out.dims)
  out.dcp <- .C_make_dcp(out.dims)[1:out.ndims]
  if(in2out) {
    # note that, when in20ut = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  if(any(names(out.dims) == "padding")) {
    out <- array(padding, unname(out.dims))
  }
  else {
    out <- vector("list", out.len)
    dim(out) <- unname(out.dims)
  }
  
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
  
  out.dcp <- .C_make_dcp(out.dims)[1:out.ndims]
  if(in2out) {
    # note that, when in20ut = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  
  return(out.dims)
  
}



