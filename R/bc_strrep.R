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
    
    .bc_strrep(x, y, mycall)
    
  }
  
)


#' @keywords internal
#' @noRd
.bc_strrep <- function(x, y, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "character"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_strrep_v(x, y, out.len)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_strrep_ov(x, y, RxC, out.dimsimp, out.len)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bc_strrep_bv(x, y, bigx, out.dimsimp, out.len)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_strrep_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}
