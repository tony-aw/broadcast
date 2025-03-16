#' Replicate Array Dimensions
#'
#' @description
#' The `rep_dim()` function
#' replicates array dimensions until the specified dimension sizes are reached,
#' and returns the array. \cr
#' \cr
#' The various broadcasting functions recycle array dimensions virtually,
#' meaning little to no additional memory is needed. \cr
#' The `rep_dim()` function,
#' however,
#' physically replicates the dimensions of an array
#' (and thus actually occupies additional memory space). \cr
#' \cr
#'
#' @param x an atomic or recursive array or matrix.
#' @param tdim an integer vector, giving the target dimension to reach.
#' 
#' @returns
#' Returns the replicated array.
#'
#'
#' @example inst/examples/rep_dim.R
#' 

#' @rdname rep_dim
#' @export
rep_dim <- function(
    x, tdim
) {

  # Prep:
  tdimlen <- length(tdim)
  if(tdimlen > length(dim(x))) {
    dim(x) <- c(dim(x), rep(1, tdimlen - length(dim(x))))
  }
  x.dim <- dim(x)
  x.dimlen <- length(dim(x))
  
  # Checks:
  if(.array.check_reduce(x, tdim)) {
    stop("reduced dimensions not allowed")
  }
  indx <- seq_len(min(length(tdim), x.dimlen))
  is_fractional <- any(
    tdim[indx]/x.dim[indx] != round(tdim[indx]/x.dim[indx])
  )
  if(is_fractional) stop("fractional recycling not allowed")
  
  # Core function:
  times <- tdim
  if(tdimlen < x.dimlen) {
    times <- c(tdim, rep(1, x.dimlen - tdimlen))
  }
  
  subs <- .rcpp_recycle_seq_mlen(x.dim, times)
  x <- do.call(function(...)x[..., drop = FALSE], subs)
  
  return(x)
  
}


#' @keywords internal
#' @noRd
.internal_drop_dims <- function(x) {
  x <- drop(x)
  if(is.null(dim(x))) {
    dim(x) <- length(x)
  }
  return(x)
}


#' @keywords internal
#' @noRd
.array.check_reduce <- function(x, tdim) {
  x.dim <- dim(x)
  x.dimlen <- length(x.dim)
  x.len <- length(x)
  tdimlen <- length(tdim)
  if(tdimlen < x.dimlen) return(TRUE)
  indx <- seq_along(min(x.dimlen, length(tdim)))
  if(any(tdim[indx] < x.dim[indx])) return(TRUE)
  if(x.len > prod(tdim)) return(TRUE)
  
  return(FALSE)
}
