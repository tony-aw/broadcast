#' Predict Broadcasted dimensions
#'
#' @description
#' `bc_dim(x, y)` gives the dimensions an array would have,
#' as the result of an broadcasted binary element-wise operation between 2 arrays
#' `x` and `y`.
#'
#' @param x,y an atomic array or matrix.
#' 
#' @returns
#' Returns the recycled array.
#'
#'
#' @example inst/examples/bc_dim.R
#' 

#' @rdname bc_dim
#' @export
bc_dim <- function(
    x, y
) {
  
  prep <- .prep_arrays(x, y)
  x <- prep[[1L]]
  y <- prep[[2L]]
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  out.dim <- .determine_out.dim(x.dim, y.dim, sys.call())
  return(out.dim)
}
