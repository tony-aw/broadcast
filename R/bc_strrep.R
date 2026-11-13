#' Broadcasted strrep
#'
#' @description
#' The `bc_strrep()` method
#' is a broadcasted form of \link[base]{strrep}. \cr \cr
#' 
#' @param x vector/array of type `character`.
#' @param y vector/array of type `integer`.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#'
#' @returns
#' A character array as a result of the broadcasted repetition operation. \cr \cr
#'
#' 
#' @example inst/examples/bc_strrep.R
#' 





#' @rdname bc_strrep
#' @export
setGeneric(
  "bc_strrep",
  function(x, y, ...) standardGeneric("bc_strrep"),
  signature = c("x", "y")
)


#' @rdname bc_strrep
#' @export
setMethod(
  "bc_strrep", c(x = "ANY", y = "ANY"),
  function(x, y) {
    
    mycall <- "bc_strrep"
    
    # checks:
    .binary_stop_general(x, y, "", mycall)
    if(!is.character(x)) {
      stop(simpleError("`x` must be a character/string array", call = mycall))
    }
    if(!is.numeric(y)) {
      stop(simpleError("`y` must be an integer array", call = mycall))
    }
    if(is.double(y)) {
      y <- as_int(y)
    }
    
    encodings <- c(
      Encoding(x[1]),
      Encoding(x[length(x)])
    )
    if(length(unique(encodings)) > 1L) {
      warning(simpleWarning("difference in encoding detected within `x`", call = mycall))
    }
    
    return(.bc_strrep(x, y, mycall))
    
  }
  
)


#' @keywords internal
#' @noRd
.bc_strrep <- function(x, y, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "character"))
  }
  
  prep <- .binary_prep(x, y)
  dimmode <- prep[[6L]]
  op <- 1L
  
  if(dimmode <= 2L) {
    out <- .rcpp_bc_strrep_2v(x, y, prep, op)
  }
  else {
    out <- .rcpp_bc_strrep_d(x, y, prep, op)
  }
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}
