#' Broadcasted Ifelse
#'
#' @description
#' The `bc_ifelse()` S4 generic method
#' performs a broadcasted form of \link[base]{ifelse}. \cr
#' 
#' @param cond `logical` vector or array with the length equal to `prod(bc_dim(yes, no))`.
#' @param yes,no conformable arrays of the same type. \cr
#' All atomic types (see \link[base]{atomic}) are supported. \cr
#' Recursive arrays of type `list` are also supported. \cr
#' since `bc_ifelse()` is an S4 generic, it can be extended to support special array classes. \cr
#' 
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
#'  - when `cond[i] == TRUE`, `out[i]` is `yes[i]`;
#'  - when `cond[i] == FALSE`, `out[i]` is `no[i]`;
#'  - when `cond[i]` is `NA`,
#'  `out[i]` is `NA` when `yes` and `no` are atomic,
#'  and `out[i]` is `list(NULL)` when `yes` and `no` are recursive. \cr \cr
#'
#'
#' @example inst/examples/bc_ifelse.R
#' 


#' @rdname bc_ifelse
#' @export
bc_ifelse <- function(cond, yes, no) {
  
  # checks:
  if(!is.array(yes) || !is.array(no)) {
    stop("`yes`, and `no` must be arrays")
  }
  if(typeof(yes) != typeof(no)) {
    stop("`yes` and `no` must be of the same type")
  }
  if(!is.logical(cond)) {
    stop("`cond` must be a logical array")
  }
  if(length(cond) != prod(bc_dim(yes, no))) {
    stop("`cond` of incorrect length")
  }
  
  # re-assign
  x <- yes
  y <- no
  
  # Prep:
  prep <- .prep_binary(x, y, sys.call())
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
    out <- .rcpp_bc_ifelse_v(cond, x, y, out.len)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_ifelse_ov(cond, x, y, RxC, out.dimsimp, out.len)
  }
  else if(dimmode == 3L){ # big-small mode
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    if(all(x.dim == out.dimsimp)) {
      bigx <- TRUE
    }
    else {
      bigx <- FALSE
    }
    out <- .rcpp_bc_ifelse_bs(
      cond, x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_ifelse_d(
      cond, x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}

