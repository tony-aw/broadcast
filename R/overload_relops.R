

#' @export
`==.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 1L, sys.call())
}


#' @export
`!=.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 2L, sys.call())
}


#' @export
`<.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 3L, sys.call())
}


#' @export
`>.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 4L, sys.call())
}



#' @export
`<=.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 5L, sys.call())
}


#' @export
`>=.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 6L, sys.call())
}


#' @keywords internal
#' @noRd
.overload_relop_equneq <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  if(is.character(e1) || is.character(e2)) {
    if(!is.character(e1)) e1 <- as_str(e1)
    if(!is.character(e2)) e2 <- as_str(e2)
    return(.bc_str_rel(e1, e2, op, abortcall))
  }
  else if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    return(.bc_cplx_rel(e1, e2, op, abortcall))
  }
  else if(is.numeric(e1) || is.numeric(e2)) {
    if(!is.double(e1)) e1 <- as_dbl(e1)
    if(!is.double(e2)) e2 <- as_dbl(e2)
    return(.bc_dec_rel(e1, e2, op, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    return(.bc_b_rel(e1, e2, op, abortcall))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @keywords internal
#' @noRd
.overload_relop_gs <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  if(is.numeric(e1) || is.numeric(e2)) {
    if(!is.double(e1)) e1 <- as_dbl(e1)
    if(!is.double(e2)) e2 <- as_dbl(e2)
    return(.bc_dec_rel(e1, e2, op, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    return(.bc_b_rel(e1, e2, op))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop(simpleError("unsupported combination of types given", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.bc_raw_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(logical(0L))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_raw_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_raw_ov(x, y, RxC, out.dimsimp, out.len, op)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bcRel_raw_bv(x, y, bigx, out.dimsimp, out.len, op)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_raw_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}
