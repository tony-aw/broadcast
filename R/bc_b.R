#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` function
#' performs broadcasted Boolean operations on 2 logical (or 32bit integer) arrays. \cr
#' \cr
#' Please note that these operations will treat the input as `logical`. \cr
#' Therefore, something like `bc.b(1, 2, "==")` returns `TRUE`,
#' because both `1` and `2` are `TRUE` when treated as `logical`. \cr
#' \cr
#' 
#' @param x,y conformable arrays of type `logical`, `integer`(32 bit), or `raw`.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean  operators: `r paste0(broadcast:::.op_b(), collapse = ", ")`. \cr
#' 
#'
#' @details
#' `bc.b()` efficiently casts the input to logical without making copies of the entire vectors/arrays. \cr
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
#' A logical array as a result of the broadcasted Boolean operation. \cr \cr
#'
#'
#' @example inst/examples/bc_b.R
#' 


#' @rdname bc.b
#' @export
bc.b <- function(x, y, op) {
  
  # checks:
  .binary_stop_general(x, y, op, sys.call())
  if(!.is_boolable(x) || !.is_boolable(y)) {
    stop("unsupported types given")
  }
  
  # get operator:
  op <- which(.op_b() == op)
  
  if(length(op)) {
    return(.bc_b(x, y, op, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_b <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(logical(0L))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
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
  
  return(out)
  
}

