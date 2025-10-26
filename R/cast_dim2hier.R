#' Cast Dimensional List into Hierarchical List
#'
#' @description
#' `cast_dim2hier()` casts a dimensional list (i.e. an array of type `list`)
#' into a hierarchical/nested list. \cr
#' \cr
#' 
#' 
#' @param x an array of type `list`.
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
#' @seealso \link{broadcast_casting} \cr
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
  .ellipsis(list(...), sys.call())
  if(!.is_list(x)) {
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
  x.dcp <- .C_make_dcp(x.dim)[1:x.ndim]
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
  
  x.dimnames <- dimnames(x)
  check_name <- .is_list(x.dimnames) &&
    .C_any_nonNULL(x.dimnames) &&
    distr.names
  
  if(check_name) {
    if(in2out) {
      x.dimnames <- rev(x.dimnames)
    }
    if(!is.null(x.dimnames[[1]])) {
      names(out) <- x.dimnames[[1]]
    }
    if(length(x.dim) > 1) {
      .rcpp_rec_dim2hier_names(out, x.dimnames, 1L, depth - 1L)
    }
    
  }
  
  return(out)
  
}
