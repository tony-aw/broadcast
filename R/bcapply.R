#' Apply Function to Pair of Arrays with Broadcasting
#'
#' @description
#' The `bcapply()` function
#' applies a function to 2 arrays element-wise with broadcasting. \cr
#' 
#' @param x,y conformable atomic or recursive vectors/arrays.
#' @param f a function that takes in exactly \bold{2} arguments,
#' and \bold{returns} a result
#' that can be stored in a single element of a recursive or atomic array.
#' @param v either `NULL`, or single string, giving the scalar type for a single iteration. \cr
#' If `NULL` (default) or \code{"list"}, the result will be a recursive array. \cr
#' If it is certain that, for every iteration,
#' `f()` always results in a \bold{single atomic scalar},
#' the user can specify the type in `v` to pre-allocate the result. \cr
#' Pre-allocating the results leads to slightly faster and more memory efficient code. \cr
#' NOTE: Incorrectly specifying `v` leads to undefined behaviour; \cr
#' when unsure, leave `v` at its default value.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#'
#' @returns
#' An atomic or recursive array with dimensions `bc_dim(x, y)`. \cr
#' Preserves some of the attributes of `x` and `y` similar to broadcasted infix operators,
#' as explained in \link{broadcast_operators}. \cr \cr
#'
#'
#' @example inst/examples/bcapply.R
#' 

#' @rdname bcapply
#' @export
setGeneric(
  "bcapply",
  function(x, y, f, ...) standardGeneric("bcapply"),
  signature = c("x", "y")
)

#' @rdname bcapply
#' @export
setMethod(
  "bcapply", c(x = "ANY", y = "ANY"),
  function(x, y, f, v = NULL) {
    
    mycall <- "bcapply"
    
    return(.bcapply(x, y, f, v, mycall))
    
  }
)


#' @keywords internal
#' @noRd
.bcapply <- function(x, y, f, v, abortcall) {
  
  # checks:
  .binary_stop_general(x, y, "", abortcall)
  if(!is.function(f)) {
    stop(simpleError("`f` must be a function", call = abortcall))
  }
  if(.n_args(f) != 2L) {
    stop(simpleError("`f` must be a function that takes in exactly 2 arguments", call = abortcall))
  }
  if(!.is_supported_type(x) || !.is_supported_type(y)) {
    stop(simpleError("input must be arrays or simple vecors", call = abortcall))
  }
  if(is.null(v)) {
    v <- "list"
  }
  if(!v %in% c("raw", "logical", "integer", "double", "complex", "character", "list")) {
    stop(simpleError("unsupported type specified for `v`", call = abortcall))
  }
  
  
  # early zero-len return:
  if(length(x) == 0L || length(y) == 0L) {
    return(vector(v, 0L))
  }
  
  
  # General prep:
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  # Allocate output:
  out <- vector(v, out.len)
  
  
  # transform function:
  fnew <- .transform_function(f)
  
  
  # Broadcast:
  
  if(dimmode == 1L) { # vector mode
    .rcpp_bcapply_v(out, x, y, out.len, fnew)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    .rcpp_bcapply_ov(out, x, y, RxC, out.dimsimp, out.len, fnew)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    .rcpp_bcapply_d(
      out, x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, fnew
    )
  }
  
  dim(out) <- out.dimorig
  
  .binary_set_attr(out, x, y)
  
  return(out)
}