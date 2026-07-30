#' Broadcasted General Relational Operators
#'
#' @description
#' The `bc.rel()` method
#' performs broadcasted general relational operations on 2 arrays. \cr
#' 
#' @param x,y conformable vectors/arrays of any atomic type.
#' @param op a single string, giving the relational operator. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_rel(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#'
#' @returns
#' A logical array as a result of the broadcasted general relational operation. \cr \cr
#'
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_rel.R
#' 

#' @rdname bc.rel
#' @export
setGeneric(
  "bc.rel",
  function(x, y, op, ...) standardGeneric("bc.rel"),
  signature = c("x", "y")
)


#' @rdname bc.rel
#' @export
setMethod(
  "bc.rel", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.rel"
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!is.atomic(x) || !is.atomic(y)) {
      stop(simpleError("only atomic arrays supported for general relational operators", call = mycall))
    }
    
    # get operator:
    op <- which(.op_rel() == op)
    
    if(length(op)) {
      return(.bc.rel(x, y, op, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
  }
)


#' @keywords internal
#' @noRd
.bc.rel <- function(x, y, op, abortcall) {
  if(op %in% c(1L, 2L)) {
    .overload_relop_equneq(x, y, op, abortcall)
  }
  else {
    .overload_relop_gs(x, y, op, abortcall)
  }
}



#' @keywords internal
#' @noRd
.bc_raw_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bcRel_raw_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_raw_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}

