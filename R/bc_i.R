#' Broadcasted Integer Numeric Operations with Extra Overflow Protection
#'
#' @description
#' The `bc.i()` function
#' performs broadcasted integer numeric operations on 2 numeric or logical arrays. \cr
#' \cr
#' Please note that these operations will treat the input as 53bit integers,
#' and will efficiently truncate when necessary. \cr
#' Therefore, something like `bc.i(1, 1.5, "==")` returns `TRUE`,
#' because `trunc(1.5)` equals `1`. \cr
#' \cr
#' 
#' @param x,y conformable logical or numeric arrays.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_int_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_int_rel(), collapse = ", ")`. \cr
#' The "gcd" operator performs the Greatest Common Divisor" operation,
#' using the Euclidean algorithm.. \cr
#' 
#' 
#' 
#' 
#'
#' @returns
#' For arithmetic operators: \cr
#' A numeric array of whole numbers,
#' as a result of the broadcasted arithmetic operation. \cr
#' Base 'R' supports 53 bit integers,
#' which thus range from approximately `-9` quadrillion to `+9` quadrillion. \cr
#' Values outside of this range will be returned as `-Inf` or `Inf`,
#' as an extra protection against integer overflow. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted integer relational comparison. \cr
#' \cr
#' 
#' 
#'
#' @example inst/examples/bc_i.R
#' 


#' @rdname bc.i
#' @export
bc.i <- function(x, y, op) {
  
  # checks:
  .binary_stop_general(x, y, op, sys.call())
  if(!.is_numeric_like(x) || !.is_numeric_like(y)) {
    stop("`x` and `y` must be numeric or logical arrays")
  }
  
  
  # make x and y integer scalars if possible:
  if(length(x) == 1L) {
    x <- .make_int53scalar(x)
  }
  if(length(y) == 1L) {
    y <- .make_int53scalar(y)
  }
  
  
  # get operator:
  op_math <- which(.op_int_math() == op)
  op_rel <- which(.op_int_rel() == op)
  
  if(length(op_math)) {
    return(.bc_int_math(x, y, op_math, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_int_rel(x, y, op_rel, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_int_math <- function(x, y, op, abortcall) {
  
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
    out <- .rcpp_bc_int_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_int_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L){ # big-small mode
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    if(all(x.dim == out.dimsimp)) {
      bigx <- TRUE
    }
    else {
      bigx <- FALSE
    }
    out <- .rcpp_bc_int_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_int_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_int_rel <- function(x, y, op, abortcall) {
  
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
    out <- .rcpp_bcRel_int_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_int_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L){ # big-small mode
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    if(all(x.dim == out.dimsimp)) {
      bigx <- TRUE
    }
    else {
      bigx <- FALSE
    }
    out <- .rcpp_bcRel_int_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_int_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
