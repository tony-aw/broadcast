#' Broadcasted Relational operators
#'
#' @description
#' The `bc()` function performs broadcasted relational operations on 2 atomic arrays. \cr
#' 
#' @param x,y conformable atomic arrays of types.
#' @param op a single string, giving the operator. \cr
#' Supported operators: ==, !=, <, >, <=, >=.
#' @param precision a single decimal number, giving the machine precision. \cr
#' Only applicable when `x` and/or `y` is of type `double`
#' 
#' 
#'
#' @returns
#' A numeric array as a result of the broadcasted arithmeric operation. \cr
#'
#'
#'
#' @example inst/examples/bc_d.R
#' 


#' @rdname bc
#' @export
bc <- function(x, y, op, prec = sqrt(.Machine$double.eps)) {
  
  if(is.numeric(x) || is.numeric(y)) {
    return(.bc_rel_dbl(x, y, op, prec, sys.call()))
  }
  
}
