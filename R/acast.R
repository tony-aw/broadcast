#' Simple and Fast Casting/Pivoting of an Array
#'
#' @description
#' The `acast()` function spreads subsets of an array margin over a new dimension. \cr
#' Written in 'C' and 'C++' for high speed and memory efficiency. \cr
#' \cr
#' Roughly speaking, `acast()` can be thought of as the "array" analogy to
#' \code{data.table::}\link[data.table]{dcast}. \cr
#' But note 2 important differences:
#' 
#'  - `acast()` works on arrays instead of data.tables.
#'  - `acast()` casts into a completely new dimension
#'  (namely `ndim(x) + 1`),
#'  instead of casting into new columns.
#' 
#' 
#' @param x an atomic or recursive array.
#' @param margin a scalar integer, specifying the margin to cast from.
#' @param grp a factor, where `length(grp) == dim(x)[margin]`, with at least `2` unique values,
#' specifying which indices of `dim(x)[margin]` belong to which group. \cr
#' Each group will be cast onto a separate index of dimension `ndim(x) + 1`. \cr
#' Unused levels of `grp` will be dropped. \cr
#' Any `NA` values or levels found in `grp` will result in an error.
#' @param fill Boolean, indicating if missing values should be filled. \cr
#' This is used in case the levels of `grp` do not have equal frequencies,
#' and thus additional values must be filled. \cr
#' If `x` is atomic but not `raw`, missing values are filled with `NA`. \cr
#' If `x` is recursive, missing values are filled with `list(NULL)`. \cr
#' If `x` is of type `raw`, uneven groupings are not supported.
#' 
#' 
#' @details
#' For the sake of illustration, consider a matrix `x` and a grouping factor `grp`. \cr
#' Let the integer scalar `k` represent a group in `grp`, such that `k` \eqn{\in} `1:nlevels(grp)`. \cr
#' Then the code \cr
#' `out = acast(x, margin = 1, grp = grp)` \cr
#' essentially performs the following for every group `k`:
#' 
#'  - copy-paste the subset `x[grp == k, ]` to the subset `out[, , k]`.
#'
#'
#' Please see the examples section
#' to get a good idea on how this function casts an array. \cr
#' A more detailed explanation of the `acast()` function
#' can be found on the website. \cr \cr
#' 
#' 
#' 
#' 
#' @returns
#' An array with the following properties:
#'  
#'  - the number of dimensions of the output array is equal to `ndim(x) + 1`;
#'  - the dimensions of the output array is equal to `c(dim(x), max(tabulate(grp))`;
#'  - the `dimnames` of the output array is equal to `c(dimnames(x), list(levels(grp)))`. \cr \cr
#' 
#' 
#' @section Back transformation: 
#' 
#' From the casted array, \cr
#' `out = acast(x, margin, grp)`, \cr
#' one can get the original `x` back by using \cr
#' `back = asplit(out, ndim(out)) |> bind_array(along = margin)`. \cr
#' Note, however, the following about the back-transformed array `back`: 
#' 
#'  - `back` will be ordered by `grp` along dimension `margin`;
#'  - if the levels of `grp` did not have equal frequencies,
#'  then `dim(back)[margin] > dim(x)[margin]`,
#'  and `back` will have more missing values than `x`. \cr \cr
#' 
#'
#' @example inst/examples/acast.R
#' 
#'  


#' @rdname acast
#' @export
acast <- function(x, margin, grp, fill = FALSE) {
  
  # first checks:
  .acast_stop_margin(margin, x, sys.call())
  margin <- as.integer(margin)
  .acast_stop_x(x, margin, sys.call())
  .acast_stop_grp(grp, x, margin, sys.call())
  .acast_stop_fill(fill, sys.call())
  grp <- droplevels(grp, exclude = NA) # drop unused or missing levels
  if(max(unclass(grp)) != nlevels(grp)) {
    stop("`grp` malformed")
  }
  
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
  
  if(!is.raw(x)) {
    fillvalue <- .return_missing(x[1L])
  }
  else {
    fillvalue <- as.raw(0L)
  }
  
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
  out.dimnames[x.ndim+1L] <- list(grp_lvls) # safe, because I used droplevels()
  dimnames(out) <- out.dimnames
  
  return(out)
  
}
