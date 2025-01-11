#' Broadcasted Operations for Character/String Arrays
#'
#' @description
#' The `bc.str()` function performs broadcasted operations on 2 character/string arrays. \cr
#' 
#' @param x,y conformable atomic arrays of typee `character`.
#' @param op a single string, giving the operator. \cr
#' Supported concatenation operators: `r paste0(broadcast:::.op_str_conc(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_str_rel(), collapse = ", ")`. \cr
#' 
#' 
#'
#' @returns
#' For concatenation operators: \cr
#' A character array as a result of the broadcasted concatenation operation. \cr
#' \cr
#' For relational operators: \cr
#' A logical array as a result of the broadcasted relational comparison. \cr
#' \cr
#'
#'
#' @example inst/examples/bc_str.R
#' 


#' @rdname bc.str
#' @export
bc.str <- function(x, y, op) {
  
  # checks:
  .stop_general(x, y, op, sys.call())
  if(!is.character(x) || !is.character(y)) {
    stop("`x` and `y` must be character/string arrays")
  }
  
  # get operator:
  op_conc <- which(.op_str_conc() == op)
  op_rel <- which(.op_str_rel() == op)
  
  if(length(op_conc)) {
    return(.bc_str_conc(x, y, op_conc, sys.call()))
  }
  else if(length(op_rel)) {
    return(.bc_str_rel(x, y, op_rel, sys.call()))
  }
  else {
    stop("given operator not supported in the given context")
  }
  
  
}



#' @keywords internal
#' @noRd
.bc_str_conc <- function(x, y, op, abortcall) {
  
  
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
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_str_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_str_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bc_str_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
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
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bcRel_str_v(x, y, out.len, op)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bcRel_str_ov(x, y, RxC, out.dimsimp, out.len, op)
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
    out <- .rcpp_bcRel_str_bs(
      x, y, by_x, by_y, dcp_x, dcp_y, as.integer(out.dimsimp), out.len, bigx, op
    )
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .make_by(x.dim, out.dimsimp)
    by_y <- .make_by(y.dim, out.dimsimp)
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
