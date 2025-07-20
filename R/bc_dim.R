#' Predict Broadcasted Dimensions
#'
#' @description
#' `bc_dim(x, y)` gives the dimensions an array would have,
#' as the result of an broadcasted binary element-wise operation between 2 arrays
#' `x` and `y`.
#'
#' @param x,y an atomic or recursive array.
#' 
#' @returns
#' Returns an integer vector giving the broadcasted dimension sizes of the result,
#' or the length of the result if its dimensions will be `NULL`. \cr \cr
#'
#'
#' @example inst/examples/bc_dim.R
#' 

#' @rdname bc_dim
#' @export
bc_dim <- function(
    x, y
) {
  
  .binary_stop_general(x, y, "", sys.call())
  prep <- .binary_prep(x, y, sys.call())
  out <- prep$out.dimorig
  if(is.null(out)) out <- prep$out.len
  return(out)
}


