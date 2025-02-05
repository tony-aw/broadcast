#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` function
#' performs broadcasted Boolean operations on 2 logical or numeric arrays. \cr
#' 
#' @param x,y conformable logical or numeric arrays.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean combiner operators: `r paste0(broadcast:::.op_b_andor(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_b_rel(), collapse = ", ")`. \cr
#' 
#'
#' @returns
#' For the boolean combiner operators: \cr
#' A logical array as a result of the broadcasted arithmetic operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#'
#'
#' @example inst/examples/bc_b.R
#' 


#' @rdname bd.b
#' @export
bc.b <- function(x, y, op) {
  
  # checks:
  .stop_general(x, y, op, sys.call())
  if(!.is_logical_like(x) || !.is_logical_like(y)) {
    stop("`x` and `y` must be logical or integer arrays")
  }
  
  # get operator:
  op_andor <- which(.op_b_andor() == op)
  op_rel <- which(.op_b_rel() == op)
  
  if(length(op_andor)) {
    return(.bc_b_andor(x, y, op_andor, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_b_rel(x, y, op_rel, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_b_andor <- function(x, y, op, abortcall) {
  
  prep <- .prep_binary(x, y, abortcall)
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


#' @keywords internal
#' @noRd
.bc_b_rel <- function(x, y, op, abortcall) {
  
  prep <- .prep_binary(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_dec_v(x, y, out.len, op, 0)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_dec_ov(x, y, RxC, out.dimsimp, out.len, op, 0)
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
    out <- .rcpp_bcRel_dec_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op, 0
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_dec_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op, 0
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
