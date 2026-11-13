#' Broadcasted Complex Numeric Operations
#'
#' @description
#' The `bc.cplx()` method
#' performs broadcasted complex numeric operations on pairs of arrays. \cr
#' \cr
#' 
#' @param x,y conformable vectors/arrays of type `complex`. \cr
#' Numeric and logical arrays will be coerced to type `complex`. \cr
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_cplx_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_cplx_rel(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' @details
#' For the `*` and `/` operators, `bc.cplx()` uses more strict `NA` checks than base 'R': \cr
#' If for an iteration,
#' `re(x)`, `im(x)`, `re(y)`, and/or `im(y)` contains `NA`,
#' than the result for that iteration is necessarily `NA_complex_`.
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
    return(.bc_cplx(x, y, op, mycall))
    
  }
)


#' @keywords internal
#' @noRd
.bc_cplx <- function(x, y, op, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, op, abortcall)
  
  # get operator:
  op_math <- .C_which_op(.op_cplx_math(), op)
  op_rel <- .C_which_op(.op_cplx_rel(), op)
  
  if(op_math) {
    return(.bc_cplx_math(x, y, op_math, abortcall))
  }
  else if(op_rel) {
    return(.bc_cplx_rel(x, y, op_rel, abortcall))
  }
  else {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.bc_cplx_math <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_arith(x, y)) {
    stop(simpleError("non-numeric argument to binary operator", call = abortcall))
  }
  if(!is.complex(x)) x <- as_cplx(x)
  if(!is.complex(y)) y <- as_cplx(y)
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "complex"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_cplx_2v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_cplx_d(x, y, prep, op)
  }
  
  
  
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_cplx_rel <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_arith(x, y)) {
    stop(simpleError("invalid comparison with given types", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bcRel_cplx_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bcRel_cplx_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bcRel_cplx_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}
