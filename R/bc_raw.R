#' Broadcasted Operations that Take Raw Arrays and Return Raw Arrays
#'
#' @description
#' The `bc.raw()` method
#' performs broadcasted operations
#' on arrays of type `raw`, and the return type is \bold{always} `raw`. \cr
#' \cr
#' For bit-wise operations, use \link{bc.bit}. \cr
#' For relational operations with logical (`TRUE`/`FALSE`/`NA`) results, use \link{bc.rel}. \cr
#' \cr
#' 
#' @param x,y conformable vectors/arrays of type raw.
#' @param op a single string, giving the operator. \cr
#' Supported operators: `r paste0(broadcast:::.op_raw_byte(), collapse = ", ")`. \cr
#' The relational operators work the same as in \link{bc.rel},
#' but with the following difference: \cr
#' a `TRUE` result is replaced with `01`,
#' and a `FALSE` result is replaced with `00`. \cr
#' The "diff" operator performs the byte equivalent of `abs(x - y)`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' `bc.raw()` \bold{always} returns an array of type `raw`. \cr
#' For the relational operators,
#' `01` codes for `TRUE` results,
#' and `00` codes for `FALSE` results. \cr
#' \cr
#' 
#' @seealso \link{broadcast_operators}
#'
#' @example inst/examples/bc_raw.R
#' 

#' @rdname bc.raw
#' @export
setGeneric(
  "bc.raw",
  function(x, y, op, ...) standardGeneric("bc.raw"),
  signature = c("x", "y")
)


#' @rdname bc.raw
#' @export
setMethod(
  "bc.raw", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.raw"
    return(.bc_raw(x, y, op, mycall))
    
  }
)
 

#' @keywords internal
#' @noRd
.bc_raw <- function(x, y, op, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, op, abortcall)
  if(!is.raw(x) || !is.raw(y)) {
    stop(simpleError("`x` and `y` must be raw arrays", call = abortcall))
  }
  
  
  # get operator:
  op <- .C_which_op(.op_raw_byte(), op)
  
  if(length(op)) {
    return(.bc_raw_byte(x, y, op, abortcall))
  }
  else {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.bc_raw_byte <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "raw"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_raw_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_raw_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_raw_d(x, y, prep, op)
  }
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}




