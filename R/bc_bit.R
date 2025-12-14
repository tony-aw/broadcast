#' Broadcasted Bit-wise Operations
#'
#' @description
#' The `bc.bit()` function
#' performs broadcasted bit-wise operations
#' on pairs of arrays, where both arrays are of type `raw` or both arrays are of type `integer`. \cr
#' \cr
#' 
#' @param x,y conformable raw or integer (32 bit) vectors/arrays.
#' @param op a single string, giving the operator. \cr
#' Supported bit-wise operators: `r paste0(c(broadcast:::.op_bit(), broadcast:::.op_rel()), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' @details
#' The "&", "|", "xor", and "nand" operators given in `bc.bit()`
#' perform BIT-WISE AND, OR, XOR, and NAND operations, respectively. \cr
#' \cr
#' The relational operators given in `bc.bit()` perform BIT-WISE relational operations:
#' 
#'  - "==" is equivalent to bit-wise `(x & y) | (!x & !y)`, but faster;
#'  - "!=" is equivalent to bit-wise `xor(x, y)`;
#'  - "<" is equivalent to bit-wise `(!x & y)`, but faster;
#'  - ">" is equivalent to bit-wise `(x & !y)`, but faster;
#'  - "<=" is equivalent to bit-wise `(!x & y) | (y == x)`, but faster;
#'  - ">=" is equivalent to bit-wise `(x & !y) | (y == x)`, but faster. \cr \cr
#'
#' The "<<" and ">>" operators perform bit-wise left-shift and right-shift,
#' respectively,
#' on `x` by unit `y`. \cr
#' For these shift operations,
#' `y` being larger than the number of bits of `x` results in an error. \cr
#' Shift operations are only supported for type of `raw`. \cr \cr
#'  
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
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    
    if(is.double(x) || is.double(y)) {
      stop(simpleError("only 32-bit integers allowed, not 53-bit integers", call = mycall))
    }
    
    if(typeof(x) != typeof(y)) {
      stop(simpleError("`x` and `y` must be of the same type", call = mycall))
    }
    check_raw <- is.raw(x) && is.raw(y)
    check_int <- is.integer(x) && is.integer(y)
    if(!check_raw && !check_int) {
      stop(simpleError("`x` and `y` must both be raw or integer arrays", call = mycall))
    }
    
    
    # get operator:
    op_andor <- which(.op_bit() == op)
    op_rel <- which(.op_rel() == op)
    
    if(length(op_andor)) {
      return(.bc_bit_andor(x, y, op_andor, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_bit_rel(x, y, op_rel, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)


#' @keywords internal
#' @noRd
.bc_bit_andor <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_bit_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_bit_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bc_bit_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_bit_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}



#' @keywords internal
#' @noRd
.bc_bit_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_bit_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_bit_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcRel_bit_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_bit_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}

