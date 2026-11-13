#' Broadcasted Decimal Numeric Operations
#'
#' @description
#' The `bc.d()` method
#' performs broadcasted decimal numeric operations on 2 numeric or logical arrays. \cr
#' \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type numeric or logical.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(c(broadcast:::.op_dec_math(), broadcast:::.op_dec_pminmax()), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(c(broadcast:::.op_dec_rel(), broadcast:::.op_dec_dist()), collapse = ", ")`. \cr
#' @param tol a single number between 0 and 0.1, giving the machine tolerance to use for the relational operators. \cr
#' Only relevant for the following operators: \cr
#' `r paste0(broadcast:::.op_dec_dist(), collapse = ", ")` \cr
#' See the
#' `r paste0(paste0("%", broadcast:::.op_dec_dist(), "%"), collapse = ", ")` operators
#' from the 'tinycodet' package for details.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' For arithmetic operators: \cr
#' A numeric array as a result of the broadcasted decimal arithmetic operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted decimal relational comparison. \cr
#' \cr
#'
#' 
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_d.R
#' 


#' @rdname bc.d
#' @export
setGeneric(
  "bc.d",
  function(x, y, op, ...) standardGeneric("bc.d"),
  signature = c("x", "y")
)


#' @rdname bc.d
#' @export
setMethod(
  "bc.d", c(x = "ANY", y = "ANY"),
  function(x, y, op, tol = sqrt(.Machine$double.eps)) {
    
    mycall <- "bc.d"
    return(.bc_d(x, y, op, tol, mycall))
  }
)


#' @keywords internal
#' @noRd
.bc_d <- function(x, y, op, tol, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, op, abortcall)
  
  if(length(x) == 1L) {
    x <- .trymake_simple_int_scalar(x)
  }
  if(length(y) == 1L) {
    y <- .trymake_simple_int_scalar(y)
  }
  
  # get operator:
  op_math <- .C_which_op(.op_dec_math(), op)
  op_rel <- .C_which_op(.op_dec_rel(), op)
  op_dist <- .C_which_op(.op_dec_dist(), op)
  op_pminmax <- .C_which_op(.op_dec_pminmax(), op)
  op_boolint_rel <- .C_which_op(.op_dec_boolint_rel(), op)
  
  if(.C_should_boolint_math(x, y, op_pminmax)) {
    return(.bc_boolint_math(x, y, op_pminmax, abortcall))
  }
  else if(.C_should_boolint_rel(x, y, op_boolint_rel)) {
    return(.bc_boolint_rel(x, y, op_boolint_rel, abortcall))
  }
  else if(op_math) {
    return(.bc_dec_math(x, y, op_math, abortcall))
  }
  else if(op_rel) {
    return(.bc_dec_rel(x, y, op_rel, abortcall))
  }
  else if(op_dist) {
    return(.bc_tolerance(x, y, op_dist, tol, FALSE, abortcall))
  }
  else if(op_pminmax) {
    return(.bc_dec_pminmax(x, y, op_pminmax, abortcall))
  }
  else {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bc_dec_math <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_arith(x, y)) {
    stop(simpleError("non-numeric argument to binary operator", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "double"))
  }
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_dec_2v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_dec_d(x, y, prep, op)
  }
  

  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_dec_rel <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_relop_gs(x, y)) {
    stop(simpleError("invalid comparison with given types", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(op <= 2L) {
    if(dimmode <= 2L) {
      out <- .rcpp_bcRelEq_dec_2v(x, y, prep, op)
    }
    else if(dimmode <= 4L) {
      out <- .rcpp_bcRelEq_dec_1v(x, y, prep, op)
    }
    else {
      out <- .rcpp_bcRelEq_dec_d(x, y, prep, op)
    }
  }
  else {
    if(dimmode <= 2L) {
      out <- .rcpp_bcRelGS_dec_2v(x, y, prep, op)
    }
    else if(dimmode <= 4L) {
      out <- .rcpp_bcRelGS_dec_1v(x, y, prep, op)
    }
    else {
      out <- .rcpp_bcRelGS_dec_d(x, y, prep, op)
    }
  }
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}

#' @keywords internal
#' @noRd
.bc_dec_pminmax <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_arith(x, y)) {
    stop(simpleError("non-numeric argument to binary operator", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "double"))
  }
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bcP_dec_2v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bcP_dec_d(x, y, prep, op)
  }
  

  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}

