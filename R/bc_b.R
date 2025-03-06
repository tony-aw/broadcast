#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` function
#' performs broadcasted Boolean operations on 2 logical (or 32bit integer) arrays. \cr
#' \cr
#' Please note that these operations will treat the input as Boolean. \cr
#' Therefore, something like `bc.b(1, 2, "==")` returns `TRUE`,
#' because both `1` and `2` are `TRUE` when cast as Boolean. \cr
#' \cr
#' 
#' @param x,y conformable logical (or 32bit integer) arrays.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean  operators: `r paste0(broadcast:::.op_b(), collapse = ", ")`. \cr
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
  .binay_stop_general(x, y, op, sys.call())
  if(!.is_logical_like(x) || !.is_logical_like(y)) {
    stop("`x` and `y` must be logical (or logical-like) arrays")
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
  
  prep <- .binay_prep(x, y, abortcall)
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
  else if(dimmode == 3L){ # big-small mode
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    if(all(x.dim == out.dimsimp)) {
      bigx <- TRUE
    }
    else {
      bigx <- FALSE
    }
    out <- .rcpp_bc_b_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_b_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}

