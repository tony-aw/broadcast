#' Broadcasted Operations for Recursive Arrays
#'
#' @description
#' The `bc.list()` function performs broadcasted operations on 2 Recursive arrays. \cr
#' 
#' @param x,y conformable Recursive arrays (i.e. arrays of type `list`).
#' @param f a function that takes in exactly \bold{2} arguments,
#' and \bold{returns} a result that can be stored in a single element of a list. \cr
#' 
#' 
#'
#' @returns
#' A recursive array. \cr
#'
#'
#' @example inst/examples/bc_list.R
#' 


#' @rdname bc.list
#' @export
bc.list <- function(x, y, f) {
  
  # checks:
  .binary_stop_general(x, y, "", sys.call())
  if(!is.list(x) || !is.list(y)) {
    stop("`x` and `y` must be recursive arrays")
  }
  if(!is.function(f)) {
    stop("`f` must be a function")
  }
  if(.n_args(f) != 2L) {
    stop("`f` must be a function that takes in exactly 2 arguments")
  }
  
  
  # general prep:
  prep <- .binary_prep(x, y, sys.call())
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
    out <- .rcpp_bc_list_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, f
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_list_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, f
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}

