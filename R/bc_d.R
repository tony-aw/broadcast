#' Broadcasted Decimal Arithmetic
#'
#' @description
#' The `bc.d()` function performs broadcasted decimal arithmetic operations on 2 atomic arrays. \cr
#' 
#' @param x,y conformable atomic arrays of types `logical`, `integer`, or `double`.
#' @param op a single string, giving the operator. \cr
#' Supported operators: +, -, *, /, ^.
#' 
#' 
#'
#' @returns
#' The list.
#'
#'
#'
#' @example inst/examples/bc_d.R
#' 


#' @rdname bd.d
#' @export
bc.d <- function(x, y, op) {
  
  # checks:
  .stop_general(x, y, sys.call())
  
  
  # general prep:
  prep <- .prep_arrays(x, y)
  x <- prep[[1L]]
  y <- prep[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  
  # Check & determine dimensions to return:
  .stop_conf_dim(x, y, sys.call())
  out.dimorig <- .determine_out.dim(x.dim, y.dim)
  out.len <- .determine_out.len(x, y, out.dimorig)
  
  
  # Simplify arrays, to reduce broadcast load:
  simp <- .simplify_arrays(x, y)
  x <- simp[[1L]]
  y <- simp[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  out.dimsimp <- .determine_out.dim(x.dim, y.dim)
  
  
  # Broadcast:
  dimmode <- .determine_dimmode(x.dim, y.dim, out.dimsimp)
  op <- .op_dbl(op, sys.call())
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_dbl_v(x, y, out.len, op)
  }
  else if(dimmode == 2L){ # big-small mode
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
    out <- .rcpp_bc_dbl_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 3L) { # orthogonal mode
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    if(x.dim[1L] > 1L) {
      xstarts <- TRUE
    }
    else {
      xstarts <- FALSE
    }
    out <- .rcpp_bc_dbl_o(
      x, y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, xstarts, op
    )
  } # dimmode == 4L not yet implemented
  else if(dimmode == 5L) { # regular array <= 8 dims mode
    
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_dbl_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  else if(dimmode == 6L) { # misc mode
    inds_x <- .make_indices(x.dim, out.dimsimp)
    inds_y <- .make_indices(y.dim, out.dimsimp)
    out <- .rcpp_bc_dbl_general(
      x, y, inds_x, inds_y, dim(x), dim(y), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
