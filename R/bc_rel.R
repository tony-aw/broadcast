
#' @keywords internal
#' @noRd
.bc_rel_dbl <- function(x, y, op, prec, abortcall) {
  
  # checks:
  .stop_general(x, y, abortcall)
  if(!.is_numeric_like(x) || !.is_numeric_like(y)) {
    stop(simpleError("incomparable types given", call = abortcall))
  }
  if(!is.numeric(prec) || length(prec) != 1L) {
    stop("`prec` must be a single decimal number", call = abortcall)
  }
  check <- prec >= 0 && prec <= 0.1
  if(!check) {
    stop("invalid number given for `prec`", call = abortcall)
  }
  
  
  # general prep:
  prep <- .prep_arrays(x, y)
  x <- prep[[1L]]
  y <- prep[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  
  # Check & determine dimensions to return:
  .stop_conf_dim(x, y, abortcall)
  out.dimorig <- .determine_out.dim(x.dim, y.dim, abortcall)
  out.len <- .determine_out.len(x, y, out.dimorig)
  
  
  # Simplify arrays, to reduce broadcast load:
  simp <- .simplify_arrays(x, y)
  x <- simp[[1L]]
  y <- simp[[2L]]
  x.dim <- dim(x)
  y.dim <- dim(y)
  out.dimsimp <- .determine_out.dim(x.dim, y.dim, abortcall)
  
  
  # Broadcast:
  dimmode <- .determine_dimmode(x, y, out.dimsimp)
  op <- .op_rel_dbl(op, abortcall)
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_dbl_v(x, y, out.len, op, prec)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_dbl_ov(x, y, RxC, out.dimsimp, out.len, op, prec)
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
    out <- .rcpp_bcRel_dbl_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op, prec
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_dbl_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op, prec
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
