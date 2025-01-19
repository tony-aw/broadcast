#' Broadcasted Operations for Numeric Arrays
#'
#' @description
#' The `bc.num()` function performs broadcasted operations on 2 numeric arrays. \cr
#' 
#' @param x,y conformable atomic arrays of types `logical`, `integer`, or `double`.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_num_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_num_rel(), collapse = ", ")`. \cr
#' @param prec a single number between 0 and 0.1, giving the machine precision to use. \cr
#' Only relevant for the following operators: \cr
#' `r paste0(broadcast:::.op_num_rel()[7:12], collapse = ", ")` \cr
#' See the
#' `r paste0(broadcast:::.op_num_rel()[7:12], collapse = ", ")` operators
#' from the 'tinycodet' package for details. \cr
#' 
#' 
#'
#' @returns
#' For arithmetic operators: \cr
#' A numeric array as a result of the broadcasted arithmetic operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#'
#'
#' @example inst/examples/bc_num.R
#' 


#' @rdname bc.num
#' @export
bc.num <- function(x, y, op, prec = sqrt(.Machine$double.eps)) {
  
  # checks:
  .stop_general(x, y, op, sys.call())
  if(!.is_numeric_like(x) || !.is_numeric_like(y)) {
    stop("`x` and `y` must be numeric or logical arrays")
  }
  
  # get operator:
  op_math <- which(.op_num_math() == op)
  op_rel <- which(.op_num_rel() == op)
  
  if(length(op_math)) {
    return(.bc_num_math(x, y, op_math, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_num_rel(x, y, op_rel, prec, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_num_math <- function(x, y, op, abortcall) {
  
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
    out <- .rcpp_bc_dbl_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_dbl_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bc_dbl_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_dbl_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_num_rel <- function(x, y, op, prec, abortcall) {
  
  # precision checks:
  if(!is.numeric(prec) || length(prec) != 1L) {
    stop("`prec` must be a single decimal number", call = abortcall)
  }
  check <- prec >= 0 && prec <= 0.1
  if(!check) {
    stop("invalid number given for `prec`", call = abortcall)
  }
  
  
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
    out <- .rcpp_bcRel_dbl_v(x, y, out.len, op, prec)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_dbl_ov(x, y, RxC, out.dimsimp, out.len, op, prec)
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
    out <- .rcpp_bcRel_dbl_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op, prec
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_dbl_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op, prec
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
