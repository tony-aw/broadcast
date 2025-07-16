
#' @keywords internal
#' @noRd
.acast_stop_margin <- function(margin, x, abortcall) {
  
  if(!.is.integer_scalar(margin)) {
    stop(simpleError("`margin` must be an integer scalar", call = abortcall))
  }
  if(margin < 1L || margin > ndim(x)) {
    stop(simpleError("`margin` out of bounds", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.acast_stop_x <- function(x, margin, abortcall) {
  if(!is.array(x)) {
    stop(simpleError("`x` must be an array", call = abortcall))
  }
  if(length(x) < 2L) {
    stop(simpleError("zero-length or singular `x` not supported", call = abortcall))
  }
  if(ndim(x) >= 15L) {
    stop(simpleError("acasting would result in an array > 16 dimensions", call = abortcall))
  }
  if(dim(x)[margin] <= 1L) {
    stop(simpleError("`dim(x)[margin]` must be >= 2", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.acast_stop_grp <- function(grp, x, margin, abortcall) {
  if(!is.factor(grp)) {
    stop(simpleError("`grp` must be a factor", call = abortcall))
  }
  if(length(grp) != dim(x)[margin]) {
    stop(simpleError("length(grp) != dim(x)[margin]", call = abortcall))
  }
  grp_n <- length(unique(grp))
  if(grp_n < 2L) {
    stop(simpleError("`grp` must have at least 2 unique values", call = abortcall))
  }
  if(anyNA(grp) || anyNA(levels(grp))) {
    stop(simpleError("`grp` cannot have NAs", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.acast_stop_fill <- function(fill, fill_val, x, abortcall) {
  if(!isTRUE(fill) && !isFALSE(fill)) {
    stop(simpleError("`fill` must be `TRUE` or `FALSE`", call = abortcall))
  }
  if(length(fill_val) != 1L) {
    stop(simpleError("`fill_val` must be a single scalar", call = abortcall))
  }
  if(is.atomic(fill_val) != is.atomic(x)) {
    stop(simpleError("`is.atomic(fill_val)` must match `is.atomic(x)`", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.acast_stop_properties <- function(x, margin, grp, fill, grp_uneven, abortcall) {
  if(isFALSE(fill) && grp_uneven) {
    stop(simpleError(
      "when `fill = FALSE`, all groups must have equal frequency", call = abortcall
    ))
  }
  if(is.raw(x) && grp_uneven) {
    stop(simpleError(
      "typeof `raw` does not support NAs, so all groups must have equal frequency", call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.acast_stop_out <- function(out.dim, abortcall) {
  maxdim <- 2^31 - 1
  maxlen <- 2^53 - 1
  if(any(out.dim > maxdim)) {
    stop(simpleError("result will exceed maximum size", call = abortcall))
  }
  if(prod(out.dim) > maxlen) {
    stop(simpleError("result will exceed maximum size", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.acast_get_dimchunksize <- function(x.ndim) {
  if(!.is.even(x.ndim)) {
    return(x.ndim + 1L)
  }
  else {
    return(x.ndim)
  }
}



#' @keywords internal
#' @noRd
.hiercast_depth <- function(x, maxdepth, recurse_classed = FALSE, abortcall) {
  
  out <- min(.rcpp_depth_range(x, maxdepth, recurse_classed))
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
.hiercast_dims <- function(x, depth, in2out, recurse_classed, abortcall) {
  dims <- integer(depth)
  names(dims) <- rep("", depth)
  dims[1] <- length(x)
  if(depth > 1) {
    for(i in 2:depth) {
      range <- .rcpp_lenrange_atdepth(x, i - 1L, recurse_classed)
      dims[i] <- range[2L]
      if(range[1L] != range[2L]) {
        names(dims)[i] <- "padding"
      }
    }
  }
  
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

