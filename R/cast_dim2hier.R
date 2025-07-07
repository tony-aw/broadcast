#' Cast Dimensional List into Hierarchical List
#'
#' @description
#' `cast_dim2hier()` casts a dimensional list
#' (i.e. recursive array)
#' into a hierarchical list. \cr
#' \cr
#' 
#' 
#' @param x a list
#' @param in2out see \link{broadcast_casting}.
#' @param distr.names `TRUE` or `FALSE`,
#' indicating if `dimnames` from `x` should be distributed over the nested elements of the output. \cr
#' See examples section for demonstration.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' A nested list. \cr
#' \cr
#' 
#'
#' @example inst/examples/cast_dim2hier.R
#' 
#'
#'

#' @rdname cast_dim2hier
#' @export
cast_dim2hier <- function(x, ...) {
  UseMethod("cast_dim2hier", x)
}


#' @rdname cast_dim2hier
#' @export
cast_dim2hier.default <- function(x, in2out = TRUE, distr.names = FALSE, ...) {
  
  # checks:
  if(!is.list(x)) {
    stop("`x` must be a list")
  }
  if(!isTRUE(in2out) && !isFALSE(in2out)) {
    stop("`in2out` must be `TRUE` or `FALSE`")
  }
  if(!isTRUE(distr.names) && !isFALSE(distr.names)) {
    stop("`distr.names` must be `TRUE` or `FALSE`")
  }
  .hiercast_check_dims(x, sys.call())
  
  
  # FUNCTION:
  x.dim <- dim(x)
  x.ndim <- depth <- ndim(x)
  x.dcp <- c(1, cumprod(x.dim))
  x.dcp <- c(1, cumprod(x.dim))[1:x.ndim]
  lens <- x.dim
  
  if(in2out) {
    # note that, when in20ut = TRUE,
    # it is needed that the dimcumprod to go from large to small
    # thus they need to be reversed, on top of the reversal of the dimensions themselves
    x.dcp <- rev(x.dcp)
    lens <- rev(lens)
  }
  
  out <- .rcpp_allocate_nestedlist(lens, 1)
  
  .rcpp_rec_dim2hier(x, out, x.dcp, 0, 1.0, depth)
  
  check_name <- is.list(dimnames(x)) &&
    !all(vapply(dimnames(x), is.null, logical(1L))) &&
    distr.names
  
  if(check_name) {
    x.dimnames <- dimnames(x)
    if(in2out) {
      x.dimnames <- rev(x.dimnames)
    }
    if(!is.null(x.dimnames[[1]])) {
      names(out) <- x.dimnames[[1]]
    }
    if(length(x.dim) > 1) {
      .rcpp_rec_dim2hier_names(out, x.dimnames, 1.0, depth - 1)
    }
    
  }
  
  return(out)
  
}
