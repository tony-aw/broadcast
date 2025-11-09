#' Cast Hierarchical List into Dimensional list
#'
#' @description
#' `cast_hier2dim()` casts a hierarchical/nested list into a dimensional list
#' (i.e. an array of type `list`). \cr
#' \cr
#' This method comes with 2 helper functions: \cr
#' \link{hier2dim} and \link{hiernames2dimnames} methods. \cr
#' See their help page for details. \cr \cr
#' 
#' 
#' @param x a nested list. \cr
#' If `x` has redundant nesting,
#' it is advisable (though not necessary) to reduce the redundant nesting using \link{dropnests}.
#' @param in2out,recurse_all see \link{broadcast_casting}.
#' @param maxdepth a single, positive integer,
#' giving the maximum depth to recurse into the list. \cr
#' The surface-level elements of a list is depth 1. \cr
#' @param padding a list of length `1`,
#' giving the padding value to use when padding is required. \cr
#' Padding is used to ensure every all slices of the same dimension in the output
#' have equal number of elements
#' (for example, all rows must have the same number of columns).
#' @param direction.names see argument `direction` from the \link{hiernames2dimnames} method.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' An array of type `list`, with the dimensions given by \link{hier2dim}. \cr
#' If the output needs padding (indicated by \link{hier2dim}),
#' the output will have more elements than `x`,
#' filled with a padding value (as specified in the `padding` argument). \cr
#' \cr
#' If `direction.names = 0` (default), the result will not have any `dimnames`; \cr
#' the `dimnames` can then still be constructed using \link{hiernames2dimnames}. \cr
#' If `direction.names` is `1` or `-1`, the result \emph{will} have `dimnames`. \cr \cr
#' 
#'
#' @seealso \link{broadcast_casting}, \link{hier2dim}, \link{hiernames2dimnames} \cr
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
cast_hier2dim.default <- function(
    x, in2out = TRUE, maxdepth = 16L, recurse_all = FALSE, padding = list(NULL),
    direction.names = 0L,
    ...
) {
  
  .ellipsis(list(...), sys.call())
  
  if(!.is_list(padding) || length(padding) > 1L) {
    stop("`padding` must be a list of length 1")
  }
  .check_direction.hiernames2dimnames(direction.names, "direction.names", sys.call())
  
  out.dims <- .hier2dim(x, in2out, maxdepth, recurse_all, sys.call())
  out.ndims <- depth <- length(out.dims)
  out.len <- prod(out.dims)
  out.dcp <- .C_make_dcp(out.dims)[1:out.ndims]
  if(in2out) {
    # note that, when in2out = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed
    out.dcp <- rev(out.dcp) 
  }
  
  if(any(names(out.dims) == "padding")) {
    out <- array(padding, unname(out.dims))
  }
  else {
    out <- vector("list", out.len)
    .rcpp_set_attr(out, "dim", unname(out.dims))
  }
  
  .rcpp_rec_cast_hier2dim(x, out, out.dcp, 0, 1.0, depth)
  
  out.dimnames <- .hiernames2dimnames(x, out.dims, in2out, maxdepth, recurse_all, direction.names)
  
  if(!is.null(out.dimnames) && .C_any_nonNULL(out.dimnames)) {
    .set_dimnames(out, out.dimnames)
  }
  
  
  return(out)
  
}
