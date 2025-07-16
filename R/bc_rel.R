#' Broadcasted General Relational Operators
#'
#' @description
#' The `bc.rel()` function
#' performs broadcasted general relational operations on 2 arrays. \cr
#' 
#' @param x,y conformable arrays of any \link{atomic} type.
#' @param op a single string, giving the relational operator. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_rel(), collapse = ", ")`.
#' @param ... further arguments passed to or from methods. \cr \cr
#'
#' @returns
#' A logical array as a result of the broadcasted general relational operation. \cr \cr
#'
#'
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
    
    # checks:
    .binary_stop_general(x, y, op, sys.call())
    if(!is.atomic(x) || !is.atomic(y)) {
      stop("only atomic arrays supported for general relational operators")
    }
    
    # get operator:
    op <- which(.op_rel() == op)
    
    if(length(op)) {
      return(.bc.rel(x, y, op, sys.call()))
    }
    else {
      stop("given operator not supported in the given context")
    }
  }
)


#' @keywords internal
#' @noRd
.bc.rel <- function(x, y, op, abortcall) {
  if(op %in% c(1L, 2L)) {
    .overload_relop_equneq(x, y, op, sys.call())
  }
  else {
    .overload_relop_gs(x, y, op, sys.call())
  }
}

