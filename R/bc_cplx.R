#' Broadcasted Complex Numeric Operations
#'
#' @description
#' The `bc.cplx()` function performs broadcasted complex numeric operations on pairs of arrays. \cr
#' \cr
#' Note that `bc.cplx()` uses more strict `NA` checks than base 'R': \cr
#' If for an element of either `x` or `y`, either the real or imaginary part is `NA` or `NaN`,
#' than the result of the operation for that element is necessarily `NA`. \cr
#' \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type `complex`.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_cplx_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_cplx_rel(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
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
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_cplx.R
#' 


#' @rdname bc.cplx
#' @export
setGeneric(
  "bc.cplx",
  function(x, y, op, ...) standardGeneric("bc.cplx"),
  signature = c("x", "y")
)


#' @rdname bc.cplx
#' @export
setMethod(
  "bc.cplx", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.cplx"
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!is.complex(x) || !is.complex(y)) {
      stop(simpleError("`x` and `y` must be complex arrays or vectors", call = mycall))
    }
    
    # get operator:
    op_math <- which(.op_cplx_math() == op)
    op_rel <- which(.op_cplx_rel() == op)
    
    if(length(op_math)) {
      return(.bc_cplx_math(x, y, op_math, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_cplx_rel(x, y, op_rel, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
    
    
  }
)


#' @keywords internal
#' @noRd
.bc_cplx_math <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(complex(0L))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
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
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_cplx_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_cplx_rel <- function(x, y, op, abortcall) {
  
  
  if(length(x) == 0L || length(y) == 0L) {
    return(logical(0L))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
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
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_cplx_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}
