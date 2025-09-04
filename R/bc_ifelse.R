#' Broadcasted Ifelse
#'
#' @description
#' The `bc_ifelse()` function
#' performs a broadcasted form of `ifelse()`. \cr
#' 
#' @param test a vector or array,
#' with the type `logical`, `integer`, or `raw`,
#' and a length equal to `prod(bc_dim(yes, no))`. \cr
#' If `yes` / `no` are of type `raw`, `test` is not allowed to contain any `NA`s.
#' @param yes,no conformable arrays of the same type. \cr
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
    
    # checks:
    .binary_stop_general(yes, no, "", sys.call())
    if(typeof(yes) != typeof(no)) {
      if(is.numeric(yes) && is.numeric(no)) {
        yes <- as_dbl(yes)
        no <- as_dbl(no)
      }
      else {
        stop("`yes` and `no` must be of the same type")
      }
    }
    if(!.is_boolable(test)) {
      stop("unsupported type given for `test`")
    }
    if(!.is_supported_type(yes) || !.is_supported_type(no)) {
      stop("input must be arrays or simple vecors")
    }
    if(length(test) == 0L) {
      return(vector(typeof(yes), 0L))
    }
    if(length(test) != prod(bc_dim(yes, no))) {
      stop("`test` of incorrect length")
    }
    
    
    # re-assign
    x <- yes
    y <- no
    
    # Prep:
    prep <- .binary_prep(x, y, sys.call())
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
    else if(dimmode == 3L) { # general mode
      
      by_x <- .C_make_by(x.dim)
      by_y <- .C_make_by(y.dim)
      dcp_x <- .C_make_dcp(x.dim)
      dcp_y <- .C_make_dcp(y.dim)
      
      out <- .rcpp_bc_ifelse_d(
        test, x, y, by_x, by_y,
        dcp_x, dcp_y, as.integer(out.dimsimp), out.len
      )
    }
    
    dim(out) <- out.dimorig
    
    if(ndim(test) <= 1L && ndim(out) <= 1L) {
      names(out) <- names(test)
    }
    else if(is.array(test) && is.array(out)) {
      if(all(dim(test) == out.dimorig)) {
        dimnames(out) <- dimnames(test)
      }
    }
    
    if(inherits(test, "broadcaster")) {
      .rcpp_set_class(out, "broadcaster")
    }
    if(is.atomic(out) && inherits(test, "mutatomic")) {
      .rcpp_set_ma(out, c("mutatomic", oldClass(out)))
    }
    
    return(out)
    
  }
)
