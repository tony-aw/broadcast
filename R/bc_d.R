#' Broadcasted Decimal Arithmetic
#'
#' @description
#' The `bc.d()` function performs broadcasted decimal arithmetic operations on 2 atomic arrays. \cr
#' 
#' @param x,y conformable atomic arrays of types `logical`, `integer`, or `double`.
#' @param op a single string, giving the operator. \cr
#' Supported operators: +, -, *, /, ^.
#' 
#' 
#'
#' @returns
#' The list.
#'
#'
#'
#' @example inst/examples/bc_d.R
#' 


#' @rdname bd.d
#' @export
bc.d <- function(x, y, op) {
  
  # checks:
  .stop_general(x, y, sys.call())
  
  # general prep:
  prep <- .prep(x, y)
  x <- prep[[1L]]
  y <- prep[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  
  # FUNCTION:
  .stop_conf_dim(x, y, sys.call())
  out.dim <- .determine_out.dim(x.dim, y.dim)
  out.len <- .determine_out.len(x, y, out.dim)
  dimmode <- .determine_dimmode(x.dim, y.dim, out.dim)
  op <- .op_dbl(op, sys.call())
  
  if(dimmode == 1L) {
    out <- .rcpp_bc_dbl_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) {
    
    by_x <- .make_by(x.dim, out.dim)
    by_y <- .make_by(y.dim, out.dim)
    dimcumprod_x <- .make_dcp(x.dim)
    dimcumprod_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_dbl_d(
      x, y, by_x, by_y,
      dimcumprod_x, dimcumprod_y, as.integer(out.dim), out.len, op
    )
  }
  else if(dimmode == 3L) {
    inds_x <- .make_indices(x.dim, out.dim)
    inds_y <- .make_indices(y.dim, out.dim)
    out <- .rcpp_bc_dbl_general(
      x, y, inds_x, inds_y, dim(x), dim(y), out.len, op
    )
  }
  
  dim(out) <- out.dim
  
  return(out)
  
}
