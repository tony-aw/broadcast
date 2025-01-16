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
  .stop_general(x, y, "", sys.call())
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
  prep <- .prep_arrays(x, y)
  x <- prep[[1L]]
  y <- prep[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  
  # Check & determine dimensions to return:
  .stop_conf_dim(x, y, sys.call())
  out.dimorig <- .determine_out.dim(x.dim, y.dim, sys.call())
  out.len <- .determine_out.len(x, y, out.dimorig)
  
  
  # Simplify arrays, to reduce broadcast load:
  simp <- .simplify_arrays(x, y)
  x <- simp[[1L]]
  y <- simp[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  out.dimsimp <- .determine_out.dim(x.dim, y.dim, sys.call())
  
  
  # Broadcast:
  dimmode <- .determine_dimmode(x, y, out.dimsimp)
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_list_v(x, y, out.len, f)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_list_ov(x, y, RxC, out.dimsimp, out.len, f)
  }
  else if(dimmode == 3L){ # big-small mode
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
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
    
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
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

