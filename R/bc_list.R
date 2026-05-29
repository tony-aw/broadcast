#' Broadcasted Operations for Recursive Arrays
#'
#' @description
#' The `bc.list()` method
#' performs broadcasted operations on 2 Recursive arrays. \cr
#' 
#' @param x,y conformable Recursive vectors/arrays (i.e. vectors/arrays of type `list`).
#' @param f a function that takes in exactly \bold{2} arguments,
#' and \bold{returns} a result that can be stored in a single element of a list.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#'
#' @returns
#' A recursive array. \cr \cr
#'
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_list.R
#' 



#' @rdname bc.list
#' @export
setGeneric(
  "bc.list",
  function(x, y, f, ...) standardGeneric("bc.list"),
  signature = c("x", "y")
)


#' @rdname bc.list
#' @export
setMethod(
  "bc.list", c(x = "ANY", y = "ANY"),
  function(x, y, f) {
    
    mycall <- "bc.list"
    
    return(.bc.list(x, y, f, mycall))
  }
)



#' @keywords internal
#' @noRd
.bc.list <- function(x, y, f, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, "", abortcall)
  if(!is.list(x) || !is.list(y)) {
    stop(simpleError("`x` and `y` must be recursive arrays", abortcall))
  }
  if(!is.function(f)) {
    stop(simpleError("`f` must be a function", abortcall))
  }
  if(.n_args(f) != 2L) {
    stop(simpleError("`f` must be a function that takes in exactly 2 arguments", abortcall))
  }
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, FALSE, "list"))
  }
  
  # general prep:
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 4L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bc_list_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, f)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_list_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, f
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
}
