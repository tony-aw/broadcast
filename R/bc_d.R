#' Broadcasted Decimal Numeric Operations
#'
#' @description
#' The `bc.d()` function
#' performs broadcasted decimal numeric operations on 2 numeric or logical arrays. \cr
#' \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type logical or numeric.
#' @param op a single string, giving the operator. \cr
#' Supported arithmetic operators: `r paste0(broadcast:::.op_dec_math(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(c(broadcast:::.op_dec_rel(), broadcast:::.op_dec_dist()), collapse = ", ")`. \cr
#' @param tol a single number between 0 and 0.1, giving the machine tolerance to use for the relational operators. \cr
#' Only relevant for the following operators: \cr
#' `r paste0(broadcast:::.op_dec_dist(), collapse = ", ")` \cr
#' See the
#' `r paste0(paste0("%", broadcast:::.op_dec_dist(), "%"), collapse = ", ")` operators
#' from the 'tinycodet' package for details.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' For arithmetic operators: \cr
#' A numeric array as a result of the broadcasted decimal arithmetic operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted decimal relational comparison. \cr
#' \cr
#'
#' 
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_d.R
#' 


#' @rdname bc.d
#' @export
setGeneric(
  "bc.d",
  function(x, y, op, ...) standardGeneric("bc.d"),
  signature = c("x", "y")
)


#' @rdname bc.d
#' @export
setMethod(
  "bc.d", c(x = "ANY", y = "ANY"),
  function(x, y, op, tol = sqrt(.Machine$double.eps)) {
    
    mycall <- "bc.d"
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!.is_numeric_like(x) || !.is_numeric_like(y)) {
      stop(simpleError("`x` and `y` must be numeric or logical arrays or vectors", call = mycall))
    }
    
    # get operator:
    op_math <- which(.op_dec_math() == op)
    op_rel <- which(.op_dec_rel() == op)
    op_dist <- which(.op_dec_dist() == op)
    
    if(length(op_math)) {
      return(.bc_dec_math(x, y, op_math, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_dec_rel(x, y, op_rel, mycall))
    }
    else if(length(op_dist)) {
      return(.bc_dec_dist(x, y, op_dist, tol, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)


#' @keywords internal
#' @noRd
.bc_dec_math <- function(x, y, op, abortcall) {
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "double"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_dec_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_dec_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bc_dec_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_dec_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_dec_rel <- function(x, y, op, abortcall) {
  
  
  # MAIN:
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_dec_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_dec_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcRel_dec_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_dec_d(
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
.bc_dec_dist <- function(x, y, op, prec, abortcall) {
  
  # precision checks:
  if(!is.numeric(prec) || length(prec) != 1L) {
    stop(simpleError("`tol` must be a single decimal number", call = abortcall))
  }
  check <- prec >= 0 && prec <= 0.1
  if(!check) {
    stop(simpleError("`tol` must be >= 0 and <= 0.1", call = abortcall))
  }
  
  # MAIN:
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcD_dec_v(x, y, out.len, op, prec)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcD_dec_ov(x, y, RxC, out.dimsimp, out.len, op, prec)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcD_dec_bv(x, y, bigx, out.dimsimp, out.len, op, prec)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcD_dec_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op, prec
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}
