#' Broadcasted Integer Numeric Operations with Extra Overflow Protection
#'
#' @description
#' The `bc.i()` function
#' performs broadcasted integer numeric operations on 2 numeric or logical arrays. \cr
#' \cr
#' Please note that these operations will treat the input as (`double` typed) integers,
#' and will efficiently truncate when necessary. \cr
#' Therefore, something like `bc.i(1, 1.5, "==")` returns `TRUE`,
#' because `trunc(1.5)` equals `1`. \cr
#' \cr
#' For regular relational operators, see \link{bc.rel}. \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type logical or numeric.
#' @param op a single string, giving the operator. \cr
#' Supported simple arithmetic operators: `r paste0(broadcast:::.op_int_math(), collapse = ", ")`. \cr
#' Supported special division arithmetic operators: `r paste0(broadcast:::.op_int_d(), collapse = ", ")`. \cr
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
#' For arithmetic operators: \cr
#' A numeric array of whole numbers,
#' as a result of the broadcasted arithmetic operation. \cr
#' Base 'R' supports integers from `-2^53` to `2^53`,
#' which thus range from approximately `-9` quadrillion to `+9` quadrillion. \cr
#' Values outside of this range will be returned as `-Inf` or `Inf`,
#' as an extra protection against integer overflow. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted integer relational comparison. \cr
#' \cr
#' 
#' 
#' @seealso \link{broadcast_operators} \cr
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
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!.is_numeric_like(x) || !.is_numeric_like(y)) {
      stop(simpleError("`x` and `y` must be numeric or logical arrays or vectors", call = mycall))
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
    op_d <- which(.op_int_d() == op)
    
    if(length(op_math)) {
      return(.bc_int_math(x, y, op_math, mycall))
    }
    else if(length(op_d)) {
      return(.bc_int_d(x, y, op_d, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_int_rel(x, y, op_rel, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
    
    
  }
)


#' @keywords internal
#' @noRd
.bc_int_math <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(numeric(0L))
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
    out <- .rcpp_bc_int_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_int_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # big-vector mode
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bc_int_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_int_d(
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
.bc_int_d <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(numeric(0L))
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
    out <- .rcpp_bcD_int_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcD_int_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # orthogonal vector mode
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcD_int_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcD_int_d(
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
.bc_int_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(logical(0L))
  }
  
  if(is.double(x) || is.double(y)) {
    if(!is.double(x)) x <- as_dbl(x)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_int_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_int_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # orthogonal vector mode
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcRel_int_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_int_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}
