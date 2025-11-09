

#' @keywords internal
#' @noRd
.hiercast_depth <- function(x, maxdepth, recurse_all = FALSE, abortcall) {
  
  out <- min(.rcpp_depth_range(x, maxdepth, recurse_all))
  if(out == 1) {
    stop(simpleError(
      "not all elements have valid nested elements",
      call = abortcall
    ))
  }
  
  return(out)
}


#' @keywords internal
#' @noRd
.hiercast_dims <- function(x, depth, in2out, recurse_all, abortcall) {
  dims <- integer(depth)
  names(dims) <- rep("", depth)
  dims[1] <- length(x)
  if(depth > 1) {
    for(i in 2:depth) {
      range <- .rcpp_lenrange_atdepth(x, i - 1L, recurse_all)
      dims[i] <- range[2L]
      if(range[1L] != range[2L]) {
        names(dims)[i] <- "padding"
      }
    }
  }
  
  .hiercast_hier2dim_check_dims_rare(dims, abortcall)
  
  
  if(in2out) {
    dims <- rev(dims)
  }
  
  
  return(dims)
}

#' @keywords internal
#' @noRd
.hiercast_check_dims <- function(x, abortcall) {
  x.dim <- dim(x)
  if(is.null(x.dim)) {
    stop("`x` has no dimensions")
  }
  if(any(x.dim == 0)) {
    stop("`x` has zero-length dimensions")
  }
  if(ndim(x) == 1) {
    stop("`x` is single-dimensional")
  }
  if(ndim(x) > 16L) {
    stop("arrays with more than 16 dimensions not supported")
  }
}


#' @keywords internal
#' @noRd
.hiercast_hier2dim_check_dims_rare <- function(dims, abortcall) {
  
  if(any(dims > (2^31 - 1))) {
    stop(simpleError(
      "list casting results in dimension overflow",
      call = abortcall
    ))
  }
  if(any(dims == 0)) {
    stop(simpleError(
      "list casting results in zero-length dimension",
      call = abortcall
    ))
  }
  if(prod(dims) > (2^52 - 1)) {
    stop(simpleError(
      "casting this list would result in length overflow",
      call = abortcall
    ))
  }
  
}
