#' Helper Functions For cast_hier2dim
#'
#' @description
#' `hier2dim()` takes a hierarchical/nested list,
#' and predicts what dimensions the list would have,
#' if casted by the \link{cast_hier2dim} function. \cr
#' \cr
#' `hiernames2dimnames()` takes a hierarchical/nested list,
#' and intelligently tries to compose dimnames for the result of \link{cast_hier2dim}. \cr
#' \cr
#' 
#' @param x a nested list. \cr
#' If `x` has redundant nesting,
#' it is advisable (though not necessary) to reduce the redundant nesting using \link{dropnests}.
#' @param in2out,recurse_all see \link{broadcast_casting}.
#' @param maxdepth a single, positive integer,
#' giving the maximum depth to recurse into the list. \cr
#' The surface-level elements of a list is depth 1. \cr
#' @param direction A single number, giving the direction in which to search for names.
#' Must be either `1` (to search from start to end) or `-1` (to search from end to start).
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
#' For `hiernames2dimnames()`: \cr
#' A list of `dimnames`; these can be assigned to the `dimnames` of the result of \link{cast_hier2dim}. \cr
#' \cr
#' 
#' 
#'
#' @seealso \link{broadcast_casting}, \link{cast_hier2dim} \cr
#' @example inst/examples/cast_hier2dim.R
#' 
#'
#'


#' @rdname hier2dim
#' @export
hier2dim <- function(x, ...) {
  UseMethod("hier2dim", x)
}

#' @rdname hier2dim
#' @export
hiernames2dimnames <- function(x, ...) {
  UseMethod("hiernames2dimnames", x)
}


#' @rdname hier2dim
#' @export
hier2dim.default <- function(x, in2out = TRUE, maxdepth = 16L, recurse_all = FALSE, ...) {
  
  .recurse_classed(list(...), sys.call())
  return(.hier2dim(x, in2out, maxdepth, recurse_all, sys.call()))
  
}


#' @rdname hier2dim
#' @export
hiernames2dimnames.default <- function(x, in2out = TRUE, maxdepth = 16L, recurse_all = FALSE, direction = 1, ...) {
  
  .recurse_classed(list(...), sys.call())
  
  if(length(direction) != 1L || !is.numeric(direction)) {
    stop("`direction` must be 1 or -1")
  }
  if(!direction %in% c(1, -1)) {
    stop("`direction` must be 1 or -1")
  }
  dim <- .hier2dim(x, in2out, maxdepth, recurse_all, sys.call())
  ndim <- length(dim)
  
  dimnames <- vector("list", ndim)
  if(in2out) {
    dimnames[ndim] <- list(names(x))
    for(i in 1:(ndim - 1L)) {
      depth <- ndim - i + 1
      dimnames[i] <- .rcpp_names_atdepth(x, direction, dim[i], depth, recurse_all)
    }
  }
  if(!in2out) {
    dimnames[1] <- list(names(x))
    for(i in 2:ndim) {
      depth <- i
      dimnames[i] <- .rcpp_names_atdepth(x, direction, dim[i], depth, recurse_all)
    }
  }
  return(dimnames)
  
}


#' @keywords internal
#' @noRd
.hier2dim <- function(x, in2out, maxdepth, recurse_all, abortcall) {
  
  # check `x`:
  if(!is.list(x)) {
   stop(simpleError("`x` must be a list", call = abortcall))
  }
  if(length(x) == 0L) {
   stop(simpleError("cannot cast zero-length list", call = abortcall))
  }
  if(length(x) > (2^31 - 1)) {
   stop(simpleError("long vectors not supported", call = abortcall))
  }
  if(!is.null(dim(x))) {
   stop(simpleError("`x` already has dimensions", call = abortcall))
  }
  
  maxdepth <- as.integer(maxdepth)
  if(length(maxdepth) != 1L || is.na(maxdepth) || maxdepth < 2L || maxdepth > 16L) {
   stop(simpleError("`maxdepth` must be a single integer between 2 and 16", call = abortcall))
  }
  
  # check binary arguments:
  if(!isTRUE(in2out) && !isFALSE(in2out)) {
   stop(simpleError("`in2out` must be `TRUE` or `FALSE`", call = abortcall))
  }
  if(!isTRUE(recurse_all) && !isFALSE(recurse_all)) {
   stop(simpleError("`recurse_all` must be `TRUE` or `FALSE`", call = abortcall))
  }
  
  # check class & nesting:
  if(!recurse_all && !is.null(attr(x, "class"))) {
   stop(simpleError("if `recurse_all` is `FALSE`, `x` cannot be a classed list", call = abortcall))
  }
  if(!.rcpp_hier2dim_surface_OK(x, recurse_all)) {
   stop(simpleError("not all surface elements have valid nested elements", call = abortcall))
  }
  
  
  # Main Function:
  
  out.ndims <- depth <- .hiercast_depth(x, maxdepth, recurse_all, sys.call())
  if(depth == 1) {
   stop(simpleError("not all surface elements have valid nested elements", call = abortcall))
  }
  out.dims <- .hiercast_dims(x, depth, in2out, recurse_all, sys.call())
  
  out.dcp <- .C_make_dcp(out.dims)[1:out.ndims]
  if(in2out) {
    # note that, when in2out = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  
  return(out.dims)
}
