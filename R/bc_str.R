#' Broadcasted String Operations
#'
#' @description
#' The `bc.str()` method
#' performs broadcasted string operations on pairs of arrays. \cr \cr
#' 
#' @param x,y conformable vectors/arrays of type `character`.
#' @param op a single string, giving the operator. \cr
#' Supported concatenation operators: `r paste0(broadcast:::.op_str_conc(), collapse = ", ")`. \cr
#' Supported relational operators: `r paste0(broadcast:::.op_str_rel(), collapse = ", ")`. \cr
#' Supported distance operators: `r paste0(broadcast:::.op_str_dist(), collapse = ", ")`. \cr
#' "lcss" stands for Longest Common Sub-String.
#' @param ... further arguments passed to or from methods. \cr \cr
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
#' @references The 'C++' code for the Levenshtein edit string distance is based on the code found in
#' \url{https://rosettacode.org/wiki/Levenshtein_distance#C++}
#' 
#' @seealso \link{broadcast_operators} \cr
#' @example inst/examples/bc_str.R
#' 





#' @rdname bc.str
#' @export
setGeneric(
  "bc.str",
  function(x, y, op, ...) standardGeneric("bc.str"),
  signature = c("x", "y")
)


#' @rdname bc.str
#' @export
setMethod(
  "bc.str", c(x = "ANY", y = "ANY"),
  function(x, y, op) {
    
    mycall <- "bc.str"
    
    # checks:
    .binary_stop_general(x, y, op, mycall)
    if(!is.character(x) || !is.character(y)) {
      stop(simpleError("`x` and `y` must be character/string arrays", call = mycall))
    }
    
    encodings <- c(
      Encoding(x[1]),
      Encoding(y[1]),
      Encoding(x[length(x)]),
      Encoding(y[length(y)])
    )
    if(length(unique(encodings)) > 1L) {
      warning(simpleWarning("difference in encoding detected between `x` and `y`", call = mycall))
    }
    
    
    # get operator:
    op_conc <- which(.op_str_conc() == op)
    op_rel <- which(.op_str_rel() == op)
    op_dist <- which(.op_str_dist() == op)
    
    if(length(op_conc)) {
      return(.bc_str_conc(x, y, op_conc, mycall))
    }
    else if(length(op_rel)) {
      return(.bc_str_rel(x, y, op_rel, mycall))
    }
    else if(length(op_dist)) {
      return(.bc_str_dist(x, y, op_dist, mycall))
    }
    else {
      stop(simpleError("given operator not supported in the given context", call = mycall))
    }
    
  }
  
)


#' @keywords internal
#' @noRd
.bc_str_conc <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 4L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bc_str_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr(out, x, y)
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.bc_str_rel <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "logical"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bcRel_str_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcRel_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}



#' @keywords internal
#' @noRd
.bc_str_dist <- function(x, y, op, abortcall) {
  
  if(length(x) == 0L || length(y) == 0L) {
    return(.binary_return_zerolen(x, y, TRUE, "integer"))
  }
  
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  if(dimmode < 5L) { # vector mode
    vectorx <- .C_dims_is_vector(x.dim)
    out <- .rcpp_bcD_str_v(x, y, x.dim, y.dim, as.integer(out.dimsimp), out.len, dimmode, vectorx, op)
  }
  else { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bcD_str_d(
      x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len, op
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  .binary_set_attr_logical(out, x, y)
  
  return(out)
  
}
