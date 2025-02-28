#' Predict Broadcasted dimensions
#'
#' @description
#' `bc_dim(x, y)` gives the dimensions an array would have,
#' as the result of an broadcasted binary element-wise operation between 2 arrays
#' `x` and `y`.
#'
#' @param x,y an atomic or recursive array.
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
  
  if(!is.array(x)) {
    x.dim <- length(x)
  }
  else {
    x.dim <- dim(x)
  }
  
  if(!is.array(y)) {
    y.dim <- length(y)
  }
  else {
    y.dim <- dim(y)
  }
  
  
  prep <- .normalize_dims(x.dim, y.dim)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
 
  out.dim <- .determine_out.dim(x.dim, y.dim, sys.call())
  return(out.dim)
}
