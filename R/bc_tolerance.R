
#' @keywords internal
#' @noRd
.bc_tolerance <- function(x, y, op, tol, guard, abortcall) {
  
  if(!.C_inputOK_relop_gs(x, y)) {
    stop(simpleError("invalid comparison with given types", call = abortcall))
  }
  
  # tolerance checks:
  if(!is.numeric(tol) || length(tol) != 1L) {
    stop(simpleError("`tol` must be a single decimal number", call = abortcall))
  }
  check <- tol >= 0 && tol <= 0.1
  if(!check) {
    stop(simpleError("`tol` must be >= 0 and <= 0.1", call = abortcall))
  }
  
  # MAIN:
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_tolerance_2v(x, y, prep, op, tol, guard)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_tolerance_1v(x, y, prep, op, tol, guard)
  }
  else {
    out <- .rcpp_bc_tolerance_d(x, y, prep, op, tol, guard)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}