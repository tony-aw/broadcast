#' Broadcasted Operations for Recursive Arrays
#'
#' @description
#' The `bc.list()` function performs broadcasted operations on 2 Recursive arrays. \cr
#' 
#' @param x,y conformable Recursive arrays (i.e. arrays of type `list`).
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
    .bc.list(x, y, f, sys.call())
  }
)



#' @keywords internal
#' @noRd
.bc.list <- function(x, y, f, abortcall) {
  # checks:
  .binary_stop_general(x, y, "", abortcall)
  if(!is.list(x) || !is.list(y)) {
    stop("`x` and `y` must be recursive arrays")
  }
  if(length(x) == 0L || length(y) == 0L) {
    return(vector("list", 0L))
  }
  if(!is.function(f)) {
    stop("`f` must be a function")
  }
  if(.n_args(f) != 2L) {
    stop("`f` must be a function that takes in exactly 2 arguments")
  }
  
  
  # general prep:
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
    out <- .rcpp_bc_list_v(x, y, out.len, f)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_list_ov(x, y, RxC, out.dimsimp, out.len, f)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_list_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, f
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
}
