#' Broadcasted Boolean Operations
#'
#' @description
#' The `bc.b()` method
#' performs broadcasted logical (or Boolean) operations on 2 arrays. \cr
#' \cr
#' Please note that these operations will treat the input as `logical`. \cr
#' Therefore, something like `bc.b(1, 2, "==")` returns `TRUE`,
#' because both `1` and `2` are `TRUE` when treated as `logical`. \cr
#' \cr
#' For regular relational operators, see \link{bc.rel}. \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type `logical`, `numeric`, or `raw`. \cr
#' Note that input with type of `double` will be coerced to `integer`.
#' @param op a single string, giving the operator. \cr
#' Supported Boolean operators: `r paste0(broadcast:::.op_b(), collapse = ", ")` \cr
#' Supported relational operators: `r paste0(broadcast:::.op_rel(), collapse = ", ")`. \cr
#' "nand" is defined here as `!(x & y)`, and "nor" is defined here as `!(x | y)`.
#' @param ... further arguments passed to or from methods. \cr \cr
#'
#'
#' @returns
#' Normally: \cr
#' A logical array/vector as a result of the broadcasted Boolean operation. \cr
#' \cr
#' If both `x` and `y` are type of raw: \cr
#' A raw array/vector as a result of the broadcasted Boolean operation,
#' where `01` codes for `TRUE` and `00` codes for `FALSE`. \cr
#' This is convenient as `raw` requires less memory space than `logical`. \cr \cr
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
    op_andor <- which(.op_b() == op)
    op_rel <- which(.op_rel() == op)
    
    if(length(op_andor)) {
      return(.bc_b_andor(x, y, op_andor, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_b_rel(x, y, op_rel, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)




#' @keywords internal
#' @noRd
.bc_b_andor <- function(x, y, op, abortcall) {
  
  if(!is.raw(x) || !is.raw(y)) {
    if(is.raw(x)) x <- as_int(x)
    if(is.raw(y)) y <- as_int(y)
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    if(is.raw(x) && is.raw(y)) {
      out.type <- "raw"
    }
    else {
      out.type <- "logical"
    }
    return(.binary_return_zerolen(x, y, TRUE, out.type))
  }
  
  prep <- .binary_prep(x, y)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bc_b_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_b_d(
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
.bc_b_rel <- function(x, y, op, abortcall) {
  
  if(!is.raw(x) || !is.raw(y)) {
    if(is.raw(x)) x <- as_int(x)
    if(is.raw(y)) y <- as_int(y)
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    if(is.raw(x) && is.raw(y)) {
      out.type <- "raw"
    }
    else {
      out.type <- "logical"
    }
    return(.binary_return_zerolen(x, y, TRUE, out.type))
  }
  
  prep <- .binary_prep(x, y)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bcRel_b_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_b_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}


