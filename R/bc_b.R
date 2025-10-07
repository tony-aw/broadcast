#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` function
#' performs broadcasted logical (or Boolean) operations on 2 arrays. \cr
#' \cr
#' Please note that these operations will treat the input as `logical`. \cr
#' Therefore, something like `bc.b(1, 2, "==")` returns `TRUE`,
#' because both `1` and `2` are `TRUE` when treated as `logical`. \cr
#' \cr
#' For regular relational operators, see \link{bc.rel}. \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type `logical`, `numeric`, or `raw`.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean  operators: `r paste0(broadcast:::.op_b(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#'
#' @details
#' `bc.b()` efficiently casts the input to logical. \cr
#' Since the input is treated as logical, the following equalities hold for `bc.b()`:
#' 
#'  - "==" is equivalent to `(x & y) | (!x & !y)`, but faster;
#'  - "!=" is equivalent to `xor(x, y)`;
#'  - "<" is equivalent to `(!x & y)`, but faster;
#'  - ">" is equivalent to `(x & !y)`, but faster;
#'  - "<=" is equivalent to `(!x & y) | (y == x)`, but faster;
#'  - ">=" is equivalent to `(x & !y) | (y == x)`, but faster. \cr \cr
#'
#'
#' @returns
#' Normally; \cr
#' A logical array/vector as a result of the broadcasted Boolean operation. \cr
#' \cr
#' If both `x` and `y` are type of raw: \cr
#' A raw array/vector as a result of the broadcasted Boolean operation,
#' where `01` codes for `TRUE` and `00` codes for `FALSE`. \cr \cr
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
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(is.double(x)) x <- as_int(x)
    if(is.double(y)) y <- as_int(y)
    if(!.is_boolable(x) || !.is_boolable(y)) {
      stop(simpleError("unsupported types given", call = mycall))
    }
    
    # get operator:
    op <- which(.op_b() == op)
    
    if(length(op)) {
      return(.bc_b(x, y, op, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)




#' @keywords internal
#' @noRd
.bc_b <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(logical(0L))
  }
  
  if(is.logical(x) || is.integer(x) || is.logical(y) || is.integer(y)) {
    if(!is.integer(x)) x <- as_int(x)
    if(!is.integer(y)) y <- as_int(y)
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_b_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_b_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_b_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}

