#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` method
#' performs broadcasted logical (or Boolean) operations on 2 arrays. \cr
#' \cr
#' Please note that these operations will treat the input as `logical`. \cr
#' Therefore, something like `bc.b(1, 2, "==")` returns `TRUE`,
#' because both `1` and `2` are `TRUE` when treated as `logical`. \cr
#' \cr
#' For regular relational operators, see \link{bc.rel}. \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type `logical`, `numeric`, or `raw`. \cr
#' Note that input with type of `double` will be coerced to `integer`.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean operators: `r paste0(broadcast:::.op_b(), collapse = ", ")`
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' Normally: \cr
#' A logical array/vector as a result of the broadcasted Boolean operation. \cr
#' \cr
#' If both `x` and `y` are type of raw: \cr
#' A raw array/vector as a result of the broadcasted Boolean operation,
#' where `01` codes for `TRUE` and `00` codes for `FALSE`. \cr
#' This is convenient as `raw` requires less memory space than `logical`. \cr \cr
#'
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_b.R
#' 

#' @rdname bc.b
#' @export
setGeneric(
  "bc.b",
  function(x, y, op, ...) standardGeneric("bc.b"),
  signature = c("x", "y")
)


#' @rdname bc.b
#' @export
setMethod(
  "bc.b", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.b"
    
    
    
    
    return(.bc_b(x, y, op, mycall))
  }
)



#' @keywords internal
#' @noRd
.bc_b <- function(x, y, op, abortcall) {
  
  
  .binary_stop_general(x, y, op, abortcall)
  if(is.double(x)) x <- as_bool(x)
  if(is.double(y)) y <- as_bool(y)
  
  if(!.is_boolable(x) || !.is_boolable(y)) {
    stop(simpleError("unsupported types given", call = abortcall))
  }
  
  
  op_b <- .C_which_op(.op_b(), op)
  if(!op_b) {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
  
  if(is.raw(x) && is.raw(y)) {
    return(.bc_b_raw(x, y, op_b, abortcall))
  }
  else {
    return(.bc_b_int(x, y, op_b, abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.bc_b_int <- function(x, y, op, abortcall) {
  
  if(is.raw(x)) x <- as_bool(x) |> suppressWarnings()
  if(is.raw(y)) y <- as_bool(y) |> suppressWarnings()
 
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_b_int_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_b_int_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_b_int_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_b_raw <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "raw"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_b_raw_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_b_raw_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_b_raw_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}

