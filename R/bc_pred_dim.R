#' Predict Broadcasted dimensions
#'
#' @description
#' `bc_pred_dim()`
#'
#' @param x,y an atomic array or matrix.
#' 
#' @returns
#' Returns the recycled array.
#'
#'
#' @example inst/examples/array_recycle.R
#' 

#' @rdname bc_pred_dim
#' @export
bc_pred_dim <- function(
    x, y
) {
  # drop scalars
  if(length(x) == 1L) {
    x <- drop(x)
  }
  if(length(y) == 1L) {
    y <- drop(y)
  }
  if(.ndims(x) <= 1L && .ndims(y) <= 1L) {
    # if both are 1d arrays, drop dimensions
    # if only one is a 1d array, DON'T drop dimensions,
    # since 1d %op% matrix is not the same as vector %op% matrix when broadcasted
    dim(x) <- NULL
    dim(y) <- NULL
  }
  x.dim <- dim(x)
  y.dim <- dim(y)
  if(!is.null(x.dim) && !is.null(y.dim)) {
    x.ndims <- length(x.dim)
    y.ndims <- length(y.dim)
    if(x.ndims > y.ndims) {
      dim(y) <- c(y.dim, rep(1L, x.ndims - y.ndims))
    }
    if(y.ndims > x.ndims) {
      dim(x) <- c(x.dim, rep(1L, y.ndims - x.ndims))
    }
  }
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  out.dim <- .determine_out.dim(x.dim, y.dim)
  return(out.dim)
}