#' Broadcasted Bit-wise Operations
#'
#' @description
#' The `bc.bit()` method
#' performs broadcasted bit-wise operations
#' on pairs of arrays, where both arrays are of type `raw` or both arrays are of type `integer`. \cr
#' \cr
#' 
#' @param x,y conformable raw or integer (32 bit) vectors/arrays.
#' @param op a single string, giving the operator. \cr
#' Supported bit-wise operators:
#' `r paste0(c(broadcast:::.op_bit(), broadcast:::.op_rel()), collapse = ", ")`. \cr
#' "nand" is defined as bit-wise `!(x & y)`,
#' and "nor" is defined as bit-wise `!(x | y)`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' @details
#' The "&", "|", "xor", "nand", and "nor" operators given in `bc.bit()`
#' perform BIT-WISE AND, OR, XOR, NAND, and NOR operations, respectively. \cr
#' \cr
#' The relational operators given in `bc.bit()` perform BIT-WISE relational operations:
#' 
#'  - "==" is equivalent to bit-wise `!xor(x, y)`, but faster;
#'  - "!=" is equivalent to bit-wise `xor(x, y)`;
#'  - "<" is equivalent to bit-wise `(!x & y)`, but faster;
#'  - ">" is equivalent to bit-wise `(x & !y)`, but faster;
#'  - "<=" is equivalent to bit-wise `(!x | y)`, but faster;
#'  - ">=" is equivalent to bit-wise `(x | !y)`, but faster. \cr \cr
#'
#' The "<<" and ">>" operators perform bit-wise left-shift and right-shift,
#' respectively,
#' on `x` by unit `y`. \cr
#' For these shift operations,
#' `y` being larger than the number of bits of `x` results in an error. \cr
#' Shift operations are only supported for type of `raw`. \cr \cr
#'
#' @note
#' `bc.bit(x, x, "nand")` and `bc.bit(x, x, "nor")` are equivalent to bit-wise negation of `x`. \cr
#' I.e.: \cr
#' `!x` if `x` is type of `raw`, or `bitwNot(x)` if `x` is type of `integer`. \cr
#' \cr
#'
#' @returns
#' For bit-wise operators: \cr
#' An array of the same type as `x` and `y`,
#' as a result of the broadcasted bit-wise operation. \cr
#' \cr
#' 
#' 
#' 
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_bit.R
#' 


#' @rdname bc.bit
#' @export
setGeneric(
  "bc.bit",
  function(x, y, op, ...) standardGeneric("bc.bit"),
  signature = c("x", "y")
)


#' @rdname bc.bit
#' @export
setMethod(
  "bc.bit", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.bit"
    
    return(.bc_bit(x, y, op, mycall))
    
  }
)


#' @keywords internal
#' @noRd
.bc_bit <- function(x, y, op, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, op, abortcall)
  
  if(is.double(x) || is.double(y)) {
    stop(simpleError("only 32-bit integers allowed, not 53-bit integers", call = abortcall))
  }
  
  if(typeof(x) != typeof(y)) {
    stop(simpleError("`x` and `y` must be of the same type", call = abortcall))
  }
  check_raw <- is.raw(x) && is.raw(y)
  check_int <- is.integer(x) && is.integer(y)
  if(!check_raw && !check_int) {
    stop(simpleError("`x` and `y` must both be raw or integer arrays", call = abortcall))
  }
  
  
  # get operator:
  op_andor <- .C_which_op(.op_bit(), op)
  op_rel <- .C_which_op(.op_rel(), op)
  
  if(op_andor) {
    return(.bc_bit_andor(x, y, op_andor, abortcall))
  }
  else if(op_rel) {
    return(.bc_bit_rel(x, y, op_rel, abortcall))
  }
  else {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bc_bit_andor <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_bit_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bc_bit_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_bit_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}



#' @keywords internal
#' @noRd
.bc_bit_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  
  if(dimmode <= 2L) {
    out <- .rcpp_bcRel_bit_2v(x, y, prep, op)
  }
  else if(dimmode <= 4L) {
    out <- .rcpp_bcRel_bit_1v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bcRel_bit_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}

