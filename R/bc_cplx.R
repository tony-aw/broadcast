#' Broadcasted Complex Numeric Operations
#'
#' @description
#' The `bc.cplx()` function performs broadcasted complex numeric operations pairs of arrays. \cr
#' \cr
#' Note that `bc.cplx()` uses more strict `NA` checks than base 'R': \cr
#' If for an element of either `x` or `y`, either the real or imaginary part is `NA` or `NaN`,
#' than the result of the operation for that element is necessarily `NA`. \cr
#' 
#' @param x,y conformable atomic arrays of type `complex`.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_cplx_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_cplx_rel(), collapse = ", ")`. \cr
#' 
#' 
#'
#' @returns
#' For arithmetic operators: \cr
#' A complex array as a result of the broadcasted arithmetic operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#'
#'
#' @example inst/examples/bc_cplx.R
#' 


#' @rdname bc.cplx
#' @export
bc.cplx <- function(x, y, op) {
  
  # checks:
  .binary_stop_general(x, y, op, sys.call())
  if(!is.complex(x) || !is.complex(y)) {
    stop("`x` and `y` must be complex arrays")
  }
  
  # get operator:
  op_math <- which(.op_cplx_math() == op)
  op_rel <- which(.op_cplx_rel() == op)
  
  if(length(op_math)) {
    return(.bc_cplx_math(x, y, op_math, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_cplx_rel(x, y, op_rel, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_cplx_math <- function(x, y, op, abortcall) {
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_cplx_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_cplx_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_cplx_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_cplx_rel <- function(x, y, op, abortcall) {
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_cplx_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_cplx_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_cplx_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
