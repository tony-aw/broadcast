#' Broadcasted Ifelse
#'
#' @description
#' The `bc_ifelse()` function
#' performs a broadcasted form of \link[base]{ifelse}. \cr
#' 
#' @param test `logical` vector or array with the length equal to `prod(bc_dim(yes, no))`.
#' @param yes,no conformable arrays of the same type. \cr
#' All \link[base]{atomic} types are supported except for the type of raw. \cr
#' Recursive arrays of type \link[base]{list} are also supported. \cr \cr
#' 
#' 
#'
#' @returns
#' The ouput, here referred to as `out`,
#' will be an array of the same type as `yes` and `no`. \cr
#' After broadcasting `yes` against `no`,
#' given any element index `i`,
#' the following will hold for the output:
#' 
#'  - when `test[i] == TRUE`, `out[i]` is `yes[i]`;
#'  - when `test[i] == FALSE`, `out[i]` is `no[i]`;
#'  - when `test[i]` is `NA`,
#'  `out[i]` is `NA` when `yes` and `no` are atomic,
#'  and `out[i]` is `list(NULL)` when `yes` and `no` are recursive. \cr \cr
#'
#'
#' @example inst/examples/bc_ifelse.R
#' 


#' @rdname bc_ifelse
#' @export
bc_ifelse <- function(test, yes, no) {
  
  # checks:
  .binary_stop_general(yes, no, "", sys.call())
  if(typeof(yes) != typeof(no)) {
    stop("`yes` and `no` must be of the same type")
  }
  if(is.raw(yes) || is.raw(no)) {
    stop("`yes` and `no` cannot be type of raw")
  }
  if(!is.logical(test)) {
    stop("`test` must be a logical array")
  }
  if(length(test) != prod(bc_dim(yes, no))) {
    stop("`test` of incorrect length")
  }
  
  # re-assign
  x <- yes
  y <- no
  
  # Prep:
  prep <- .binary_prep(x, y, sys.call())
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  # Broadcast:
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_ifelse_v(test, x, y, out.len)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_ifelse_ov(test, x, y, RxC, out.dimsimp, out.len)
  }
  else if(dimmode == 3L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_ifelse_d(
      test, x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}

