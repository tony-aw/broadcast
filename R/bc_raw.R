#' Broadcasted Operations that Take Raw Arrays and Return Raw Arrays
#'
#' @description
#' The `bc.raw()` method
#' performs broadcasted operations
#' on arrays of type `raw`, and the return type is \bold{always} `raw`. \cr
#' \cr
#' For bit-wise operations, use \link{bc.bit}. \cr
#' For relational operations with logical (`TRUE`/`FALSE`/`NA`) results, use \link{bc.rel}. \cr
#' \cr
#' 
#' @param x,y conformable vectors/arrays of type raw.
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
    
    mycall <- "bc.raw"
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!is.raw(x) || !is.raw(y)) {
      stop(simpleError("`x` and `y` must be raw arrays", call = mycall))
    }
    
    
    # get operator:
    op <- which(.op_raw_byte() == op)
    
    if(length(op)) {
      return(.bc_raw_byte(x, y, op, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)
 


#' @keywords internal
#' @noRd
.bc_raw_byte <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "raw"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bc_raw_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_raw_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}




