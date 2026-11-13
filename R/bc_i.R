#' Broadcasted Integer Operations
#'
#' @description
#' The `bc.i()` method
#' performs broadcasted integer operations on 2 numeric or logical arrays. \cr
#' \cr
#' Please note that these operations will treat the input as (53 or 32 bit) integers,
#' and will efficiently truncate using `trunc2int53()` when necessary. \cr
#' Therefore, something like `bc.i(1, 1.5, "==")` returns `TRUE`,
#' because `trunc(1.5)` equals `1`. \cr
#' \cr
#' For regular relational operators, see \link{bc.rel}. \cr
#' \cr
#' \cr
#' 
#' @param x,y conformable vectors/arrays of type logical or numeric.
#' @param op a single string, giving the operator. \cr
#' Supported integer operators: `r paste0(broadcast:::.op_int_main(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_int_rel(), collapse = ", ")`. \cr
#' The "gcd" operator performs the "Greatest Common Divisor" operation,
#' using the Euclidean algorithm.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' 
#'
#' @returns
#' 
#' For `trunc2int53()`: \cr
#' Base 'R' supports 53 bit integers from `-1 * (2^53 - 1)` to `2^53 - 1`,
#' which thus range from approximately `-9` quadrillion to `+9` quadrillion. \cr
#' `trunc2int53()` will truncate a numeric vector (like `trunc()`),
#' and replace all values above `2^53 - 1` with `Inf`,
#' and values below `-1 * (2^53 - 1)` with `-Inf`. \cr
#' \cr
#' 
#' For integer operators: \cr
#' A numeric (`double` or `integer`, depending on the situation) array of whole numbers,
#' as a result of the broadcasted arithmetic operation. \cr
#' Values outside of the 53 bit integer range will be returned as `-Inf` or `Inf`,
#' as in `trunc2int53()`,
#' as an extra protection against integer precision loss. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted integer relational comparison. \cr
#' The relational operators efficiently truncate the input during broadcasting using `trunc2int53()`. \cr
#' \cr
#' 
#' 
#' @seealso \link{broadcast_operators} \cr
#' 
#' @references
#' On the integer division and remainder (modulo) computations, %% and %/%: \cr
#' \url{https://en.wikipedia.org/wiki/Modulo}
#' 
#' @example inst/examples/bc_i.R
#' 



#' @rdname bc.i
#' @export
setGeneric(
  "bc.i",
  function(x, y, op, ...) standardGeneric("bc.i"),
  signature = c("x", "y")
)


#' @rdname bc.i
#' @export
setMethod(
  "bc.i", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.i"
    return(.bc_i(x, y, op, mycall))
    
  }
)


#' @rdname bc.i
#' @export
trunc2int53 <- function(x) {
  if(is.logical(x) || is.integer(x)) {
    return(x)
  }
  else if(is.double(x)) {
    out <- .C_trunc2int53(x)
    x.attr <- attributes(x)
    if(!is.null(x.attr)) {
      for(i in names(x.attr)) {
        .rcpp_set_attr(out, i, attr(x, i, TRUE))
      }
    }
    return(out)
  }
  else {
    stop("unsupported type given")
  }
}


#' @keywords internal
#' @noRd
.bc_i <- function(x, y, op, abortcall) {
  
  # general checks:
  .binary_stop_general(x, y, op, abortcall)
  if(is.complex(x) || is.complex(y)) {
    stop(simpleError(
      "operator not supported for type `complex`",
      call = abortcall
    ))
  }
  
  
  # make x and y simple integer scalars if possible:
  if(length(x) == 1L) {
    x <- .trymake_simple_int_scalar(x)
  }
  if(length(y) == 1L) {
    y <- .trymake_simple_int_scalar(y)
  }
  
  
  # get operator:
  op_main <- .C_which_op(.op_int_main(), op)
  op_rel <- .C_which_op(.op_int_rel(), op)
  op_boolint_math <- .C_which_op(.op_int_boolint_math(), op)
  
  if(.C_should_boolint_math(x, y, op_boolint_math)) {
    return(.bc_boolint_math(x, y, op_boolint_math, abortcall))
  }
  else if(.C_should_boolint_rel(x, y, op_rel)) {
    return(.bc_boolint_rel(x, y, op_rel, abortcall))
  }
  else if(op_main) {
    return(.bc_int_main(x, y, op_main, abortcall))
  }
  else if(op_rel) {
    return(.bc_tolerance(x, y, op_rel, 0.0, TRUE, abortcall))
  }
  else {
    stop(simpleError("given operator not supported in the given context", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bc_int_main <- function(x, y, op, abortcall) {
  
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
    out <- .rcpp_bc_int_2v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_int_d(x, y, prep, op)
  }
  
  
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}

