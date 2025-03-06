#' Broadcasted String Operations
#'
#' @description
#' The `bc.str()` function
#' performs broadcasted string operations on pairs of arrays. \cr \cr
#' 
#' @param x,y conformable atomic arrays of typee `character`.
#' @param op a single string, giving the operator. \cr
#' Supported concatenation operators: `r paste0(broadcast:::.op_str_conc(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_str_rel(), collapse = ", ")`. \cr
#' Supported distance operators: `r paste0(broadcast:::.op_str_dist(), collapse = ", ")`. \cr
#' 
#'
#' @returns
#' For concatenation operation: \cr
#' A character array as a result of the broadcasted concatenation operation. \cr
#' \cr
#' For relational operation: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#' For distance operation: \cr
#' An integer array as a result of the broadcasted distance measurement. \cr
#' \cr
#'
#'
#' @example inst/examples/bc_str.R
#' 


#' @rdname bc.str
#' @export
bc.str <- function(x, y, op) {
  
  # checks:
  .binay_stop_general(x, y, op, sys.call())
  if(!is.character(x) || !is.character(y)) {
    stop("`x` and `y` must be character/string arrays")
  }
  encodings <- c(
    Encoding(x[1]),
    Encoding(y[1]),
    Encoding(x[length(x)]),
    Encoding(y[length(y)])
  )
  if(length(unique(encodings)) > 1L) {
    warning("difference in encoding detected between `x` and `y`")
  }
    
  
  # get operator:
  op_conc <- which(.op_str_conc() == op)
  op_rel <- which(.op_str_rel() == op)
  op_dist <- which(.op_str_dist() == op)
  
  if(length(op_conc)) {
    return(.bc_str_conc(x, y, op_conc, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_str_rel(x, y, op_rel, sys.call()))
  }
  else if(length(op_dist)) {
    return(.bc_str_dist(x, y, op_dist, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
}



#' @keywords internal
#' @noRd
.bc_str_conc <- function(x, y, op, abortcall) {
  
  prep <- .binay_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_str_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_str_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bc_str_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bc_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_str_rel <- function(x, y, op, abortcall) {
  
  prep <- .binay_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_str_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_str_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bcRel_str_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcRel_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}



#' @keywords internal
#' @noRd
.bc_str_dist <- function(x, y, op, abortcall) {
  
  prep <- .binay_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  # x.len <- prep[[3L]]
  # y.len <- prep[[4L]]
  out.dimorig <- prep[[5L]]
  out.dimsimp <- prep[[6L]]
  out.len <- prep[[7L]]
  dimmode <- prep[[8L]]
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcDist_str_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcDist_str_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bcDist_str_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim)
    by_y <- .make_by(y.dim)
    dcp_x <- .make_dcp(x.dim)
    dcp_y <- .make_dcp(y.dim)
    
    out <- .rcpp_bcDist_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  dim(out) <- out.dimorig
  
  return(out)
  
}
