#' Broadcasted Ifelse
#'
#' @description
#' The `bc_ifelse()` method
#' performs a broadcasted form of \link[base]{ifelse}. \cr
#' 
#' @param test a vector or array,
#' with the type `logical`, `integer`, or `raw`,
#' and a length equal to `prod(bc_dim(yes, no))`. \cr
#' If `yes` / `no` are of type `raw`, `test` is not allowed to contain any `NA`s.
#' @param yes,no conformable vectors/arrays of the same type. \cr
#' All \link[base]{atomic} types are supported. \cr
#' Recursive arrays of type \link[base]{list} are also supported.
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#'
#' @returns
#' The output, here referred to as `out`,
#' will be an array of the same type as `yes` and `no`. \cr
#' If `test` has the same dimensions as `bc_dim(yes, no)`,
#' then `out` will also have the same dimnames as `test`. \cr
#' If `test` is a \link{broadcaster}, then `out` will also be a \link{broadcaster}. \cr
#' \cr
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
setGeneric(
  "bc_ifelse",
  function(test, yes, no, ...) standardGeneric("bc_ifelse"),
  signature = c("test", "yes", "no")
)

#' @rdname bc_ifelse
#' @export
setMethod(
  "bc_ifelse", c(test = "ANY", yes = "ANY", no = "ANY"),
  function(test, yes, no) {
    
    mycall <- "bc_ifelse"
    return(.bc_ifelse(test, yes, no, mycall))
    
    
  }
)


#' @keywords internal
#' @noRd
.bc_ifelse <- function(test, yes, no, abortcall) {
  # checks:
  .binary_stop_general(yes, no, "", abortcall)
  if(typeof(yes) != typeof(no)) {
    if(is.numeric(yes) && is.numeric(no)) {
      yes <- as_dbl(yes)
      no <- as_dbl(no)
    }
    else {
      stop(simpleError("`yes` and `no` must be of the same type", call = abortcall))
    }
  }
  if(!.is_boolable(test)) {
    stop(simpleError("unsupported type given for `test`", call = abortcall))
  }
  if(!.is_supported_type(yes) || !.is_supported_type(no)) {
    stop(simpleError("input must be arrays or simple vecors", call = abortcall))
  }
  if(length(test) == 0L) {
    out <- vector(typeof(yes), 0L)
    broadcaster(out) <- broadcaster(test)
    comment(out) <- comment(test)
    return(out)
  }
  if(length(test) != prod(bc_dim(yes, no))) {
    stop(simpleError("`test` of incorrect length", call = abortcall))
  }
  
  
  # re-assign
  x <- yes
  y <- no
  
  # Prep:
  prep <- .binary_prep(x, y, abortcall)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  out.dimorig <- prep[[3L]]
  out.dimsimp <- prep[[4L]]
  out.len <- prep[[5L]]
  dimmode <- prep[[6L]]
  
  # Broadcast:
  
  if(dimmode == 1L) { # vector mode
    out <- .rcpp_bc_ifelse_v(test, x, y, out.len)
  }
  else if(dimmode == 2L) { # orthogonal vector mode
    RxC <- x.dim[1L] != 1L # check if `x` is a column-vector (and thus y is a row-vector)
    out <- .rcpp_bc_ifelse_ov(test, x, y, RxC, out.dimsimp, out.len)
  }
  else if(dimmode == 3L) {
    bigx <- .C_dims_allge(x.dim, y.dim)
    out <- .rcpp_bc_ifelse_bv(test, x, y, bigx, out.dimsimp, out.len)
  }
  else if(dimmode == 4L) { # general mode
    
    by_x <- .C_make_by(x.dim)
    by_y <- .C_make_by(y.dim)
    dcp_x <- .C_make_dcp(x.dim)
    dcp_y <- .C_make_dcp(y.dim)
    
    out <- .rcpp_bc_ifelse_d(
      test, x, y, by_x, by_y,
      dcp_x, dcp_y, as.integer(out.dimsimp), out.len
    )
  }
  
  .rcpp_set_attr(out, "dim", out.dimorig)
  
  if(ndim(test) <= 1L && ndim(out) <= 1L) {
    .rcpp_set_attr(out, "names", names(test))
  }
  else if(is.array(test) && is.array(out)) {
    if(all(dim(test) == out.dimorig)) {
      .set_dimnames(out, dimnames(test))
    }
  }
  
  if(inherits(test, "broadcaster")) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  if(is.atomic(out) && inherits(test, "mutatomic")) {
    .rcpp_set_ma(out, c("mutatomic", oldClass(out)))
  }
  if(!is.null(comment(test))) {
    .rcpp_set_attr(out, "comment", comment(test))
  }
  
  return(out)
}
