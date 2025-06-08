#' Broadcasted Byte and Relational Operations on Raw Arrays
#'
#' @description
#' The `bc.raw()` function
#' performs broadcasted byte- and relational operations
#' on arrays of type `raw`. \cr
#' \cr
#' For bit-wise operations, use \link{bc.bit}. \cr
#' For logical operations, use \link{bc.b} \cr
#' \cr
#' 
#' @param x,y conformable raw vectors or arrays.
#' @param op a single string, giving the operator. \cr
#' Supported byte operators: `r paste0(broadcast:::.op_raw_byte(), collapse = ", ")`. \cr
#' The "diff" operator performs the byte equivalent of `abs(x - y)`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_raw_rel(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' For the byte operators: \cr
#' A array of type `raw`,
#' as a result of the broadcasted byte operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#' 
#' 
#'
#' @example inst/examples/bc_raw.R
#' 



#' @rdname bc.raw
#' @export
setGeneric(
  "bc.raw",
  function(x, y, op, ...) standardGeneric("bc.raw"),
  signature = c("x", "y")
)


#' @rdname bc.raw
#' @export
setMethod(
  "bc.raw", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    # checks:
    .binary_stop_general(x, y, op, sys.call())
    if(!is.raw(x) || !is.raw(y)) {
      stop("`x` and `y` must be raw arrays")
    }
    
    
    # get operator:
    op_bit <- which(.op_raw_byte() == op)
    op_rel <- which(.op_raw_rel() == op)
    
    if(length(op_bit)) {
      return(.bc_raw_byte(x, y, op_bit, sys.call()))
    }
    else if(length(op_rel)) {
      return(.bc_raw_rel(x, y, op_rel, sys.call()))
    }
    else {
      stop("given operator not supported in the given context")
    }
  }
)
 


#' @keywords internal
#' @noRd
.bc_raw_byte <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(raw(0L))
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
    out <- .rcpp_bc_raw_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_raw_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_raw_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_class(out, "broadcaster")
  }
  
  .binary_set_ma(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_raw_rel <- function(x, y, op, abortcall) {
  
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
    out <- .rcpp_bcRel_raw_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_raw_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_raw_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_class(out, "broadcaster")
  }
  
  return(out)
  
}

