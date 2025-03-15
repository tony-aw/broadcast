#' Simple and Fast Casting/Pivoting of an Array
#'
#' @description
#' The `acast()` function spreads subsets of an array margin over a new dimension. \cr
#' \cr
#' `acast()` is somewhat analogous to \code{data.table::}\link[data.table]{dcast},
#' but it has 2 important differences:
#' 
#'  - `acast()` works on arrays instead of data.tables.
#'  - `acast()` casts into a completely new dimension
#'  (namely \link{ndim}`(x) + 1L`),
#'  instead of casting into more columns.
#' 
#' 
#' @param x an atomic or recursive array.
#' @param margin a scalar integer, specifying the margin to cast from.
#' @param grp a factor, specifying the indices on the `margin`.
#' @param fill Boolean, indicating if missing values should be filled. \cr
#' This is used in case the levels of `grp` do not have equal frequencies,
#' and thus additional values must be filled. \cr
#' If `x` is atomic but not `raw`, missing values are filled with `NA`. \cr
#' If `x` is recursive, missing values are filled with `list(NULL)`. \cr
#' If `x` is of type `raw`, uneven groupings are not supported.
#' 
#'  
#' @details
#' A more detailed explanation of the `acast()` function can be found on the website. \cr
#' 
#' 
#' 
#' 
#' @returns
#' An array with the following properties:
#'  
#'  - `ndim(out) = ndim(x) + 1l`;
#'  - dimensions equal to `c(dim(x), max(tabulate(grp))`;
#'  - dimnames equal to `c(dimnames(x), list(levels(grp)))`;
#'
#' 
#'
#' @example inst/examples/acast.R
#' 
#'  


#' @rdname acast
#' @export
acast <- function(x, margin, grp, fill = FALSE) {
  
  # first checks:
  .acast_prestop(x, margin, sys.call())
  margin <- as.integer(margin)
  .acast_stop_x(x, margin, sys.call())
  .acast_stop_margin(margin, x, sys.call())
  .acast_stop_grp(grp, x, margin, sys.call())
  .acast_stop_fill(fill, sys.call())
  
  
  # make grp params:
  grp_lvls <- levels(grp)
  grp <- unclass(grp)
  grp_tab <- tabulate(grp)
  grp_mode <- max(grp_tab)
  grp_uneven <- any(grp_tab != grp_mode)
  grp_n <- length(unique(grp))
  
  
  # check special properties:
  .acast_stop_properties(x, margin, grp, fill, grp_uneven, sys.call())
  
  
  # get x properties:
  x.dim <- dim(x)
  x.ndim <- ndim(x)
  x.dimchunk <- c(x.dim, rep(1L, 16L - x.ndim))
  newdim <- x.ndim + 1L
  
  
  # create output:
  out.dim <- c(dim(x), grp_n)
  out.dim[margin] <- grp_mode
  out.ndim <- ndim(x) + 1L
  out.dimchunk <- c(out.dim, rep(1L, 16L - out.ndim))
  fillvalue <- .return_missing(x[1L])
  .acast_stop_out(out.dim, sys.call())
  
  out <- array(fillvalue, out.dim)
  
  
  # pre params:
  subs <- lapply(1:16L, \(i)1:x.dimchunk[i])
  starts <- rep(0L, 16L)
  lens <- lengths(subs)
  dcp_out <- cumprod(c(1, out.dimchunk))[1:16]
  dcp_x <- cumprod(c(1, x.dimchunk))[1:16]
  
  
  # CORE function:
  rcpp_acast(out, x, starts, lens, subs, dcp_out, dcp_x, grp, grp_n, margin, newdim)
  
  
  # make dimnames:
  out.dimnames <- rep(list(NULL), out.ndim)
  out.dimnames[1:x.ndim] <- dimnames(x)
  out.dimnames[x.ndim+1L] <- list(grp_lvls)
  dimnames(out) <- out.dimnames
  
  return(out)
  
}
