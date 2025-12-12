#' Simple and Fast Casting/Pivoting of an Array
#'
#' @description
#' The `acast()` function spreads subsets of an array margin over a new dimension. \cr
#' \cr
#' Roughly speaking, `acast()` can be thought of as the "array" analogy to
#' \code{data.table::dcast()}. \cr
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
#' @param fill Boolean. \cr
#' When factor `grp` is unbalanced (i.e. has unequally sized groups)
#' the result will be an array where some slices have missing values, which need to be filled.
#' If `fill = TRUE`, an unbalanced `grp` factor is allowed,
#' and missing values will be filled with `fill_val`. \cr
#' If `fill = FALSE` (default), an unbalanced `grp` factor is not allowed,
#' and providing an unbalanced factor for `grp` produces an error.
#' @param fill_val scalar of the same type of `x`,
#' giving value to use to fill in the gaps when `fill = TRUE`. \cr
#' The `fill_val` argument is ignored when `fill = FALSE`. \cr
#' If `fill_val` is missing, it is specified as follows: \cr
#'  - If `x` is of type `raw` and `fill = TRUE`, `fill_val` is not allowed to be missing, and an error is returned;
#'  - If `x` is atomic but not `raw`, `fill_val` is set to `NA`;
#'  - If `x` is of type `list`, `fill_val` is set to `list(NULL)`. \cr \cr
#' @param ... further arguments passed to or from methods.
#' 
#' 
#' @details
#' For the sake of illustration, consider a matrix `x` and a grouping factor `grp`. \cr
#' Let the integer scalar `k` represent a group in `grp`, such that `k` \eqn{\in} `1:nlevels(grp)`. \cr
#' Then the code \cr
#' `out <- acast(x, margin = 1, grp = grp)` \cr
#' essentially performs the following for every group `k`:
#' 
#'  - copy-paste the subset `x[grp == k, ]` to the subset `out[, , k]`.
#'
#'
#' Please see the examples section
#' to get a good idea on how this function casts an array. \cr \cr
#' 
#' 
#' 
#' 
#' @returns
#' An array with dimensions `c(dim(x), max(tabulate(grp))`. \cr \cr
#' 
#' @section Back transformation: 
#' 
#' From the casted array, \cr
#' `out <- acast(x, margin, grp)`, \cr
#' one can get the original `x` back by using \cr
#' `back <- asplit(out, ndim(out)) |> bind_array(along = margin)`. \cr
#' Note, however, the following about the back-transformed array `back`: 
#' 
#'  - `back` will be ordered by `grp` along dimension `margin`;
#'  - if the levels of `grp` did not have equal frequencies,
#'  then `dim(back)[margin] > dim(x)[margin]`,
#'  and `back` will have more missing values than `x`. \cr \cr
#' 
#'
#' @seealso \link{broadcast_casting} \cr
#' @example inst/examples/acast.R
#' 
#'  

#' @rdname acast
#' @export
acast <- function(x, ...) {
  UseMethod("acast", x)
}

#' @rdname acast
#' @export
acast.default <- function(
    x, margin, grp, fill = FALSE,
    fill_val,
    ...
) {
  
  # first checks:
  if(!is.array(x)) {
    stop("`x` must be an array")
  }
  .ellipsis(list(...), sys.call())
  if(missing(fill_val)) {
    if(is.raw(x) && isTRUE(fill)) {
      stop("if `x` is of type raw and `fill = TRUE`, `fill_val` must be specified explicitly")
    }
    else if(is.atomic(x)) {
      fill_val <- NA
    }
    else if(is.list(x)) {
      fill_val <- list(NULL)
    }
  }
  if(is.null(fill_val)) fill_val <- list(NULL)
  .acast_stop_margin(margin, x, sys.call())
  margin <- as.integer(margin)
  .acast_stop_x(x, margin, sys.call())
  .acast_stop_grp(grp, x, margin, sys.call())
  .acast_stop_fill(fill, fill_val, x, sys.call())
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
  newdim <- x.ndim + 1L
  
  # determine dimchunksize:
  dimchunksize <- 16L
  x.dimchunk <- c(x.dim, rep(1L, dimchunksize - x.ndim))
  
  
  
  # create output:
  out.dim <- c(dim(x), grp_n)
  out.dim[margin] <- grp_mode
  out.ndim <- ndim(x) + 1L
  out.dimchunk <- c(out.dim, rep(1L, dimchunksize - out.ndim))
  
  coerce <- .type_alias_coerce(typeof(x), sys.call())
  fillvalue <- coerce(fill_val)
  
  .acast_stop_out(out.dim, sys.call())
  
  out <- array(fillvalue, out.dim)
  .acast_set_dimnames(out, x, margin, grp_lvls)
  
  
  # pre params:
  subs <- lapply(1:dimchunksize, \(i)1:x.dimchunk[i])
  starts <- rep(0L, dimchunksize)
  lens <- lengths(subs)
  dcp_out <- .C_make_dcp(out.dimchunk)[1:dimchunksize]
  dcp_x <- .C_make_dcp(x.dimchunk)[1:dimchunksize]
  
  
  # CORE function:
  .rcpp_acast(out, x, starts, lens, subs, out.dimchunk, dcp_out, dcp_x, grp, grp_n, margin, newdim)
  
  if(broadcaster(x)) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  
  return(out)
  
}
