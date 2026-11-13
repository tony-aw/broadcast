
#' @keywords internal
#' @noRd
.bc_boolint_math <- function(x, y, op, abortcall) {
  
  if(!.C_inputOK_arith(x, y)) {
    stop(simpleError("non-numeric argument to binary operator", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "integer"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_boolint_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_boolint_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_boolint_d(x, y, prep, op)
  }
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_boolint_rel <- function(x, y, op, abortcall) {
  
  
  if(!.C_inputOK_relop_gs(x, y)) {
    stop(simpleError("invalid comparison with given types", call = abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bcRel_boolint_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bcRel_boolint_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bcRel_boolint_d(x, y, prep, op)
  }
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}


