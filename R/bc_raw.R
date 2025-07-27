#' Broadcasted Operations that Take Raw Arrays and Return Raw Arrays
#'
#' @description
#' The `bc.raw()` function
#' performs broadcasted operations
#' on arrays of type `raw`, and the return type is \bold{always} `raw`. \cr
#' \cr
#' For bit-wise operations, use \link{bc.bit}. \cr
#' For relational operations with logical (`TRUE`/`FALSE`/`NA`) results, use \link{bc.rel}. \cr
#' \cr
#' 
#' @param x,y conformable raw vectors or arrays.
#' @param op a single string, giving the operator. \cr
#' Supported operators: `r paste0(broadcast:::.op_raw_byte(), collapse = ", ")`. \cr
#' The relational operators work the same as in \link{bc.rel},
#' but with the following difference: \cr
#' a `TRUE` result is replaced with `01`,
#' and a `FALSE` result is replaced with `00`. \cr
#' The "diff" operator performs the byte equivalent of `abs(x - y)`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' `bc.raw()` \bold{always} returns an array of type `raw`. \cr
#' For the relational operators,
#' `01` codes for `TRUE` results,
#' and `00` codes for `FALSE` results. \cr
#' \cr
#' 
#' @seealso \link{broadcast_operators}
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
    op <- which(.op_raw_byte() == op)
    
    if(length(op)) {
      return(.bc_raw_byte(x, y, op, sys.call()))
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
  
  .binary_set_attr(out, x, y)
  
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


