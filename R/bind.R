#' Dimensional Binding of Objects
#'
#' @description
#' The `bind_`implementations provide dimensional binding functionalities. \cr
#' \cr
#' The following implementations are available:
#' 
#'  - `bind_mat()` binds dimensionless (atomic/recursive) vectors and (atomic/recursive) matrices row- or column-wise. \cr
#'  Returns a matrix. \cr
#'  Allows for linear/vector recycling.
#'  - `bind_array()` binds (atomic/recursive) arrays and (atomic/recursive) matrices. \cr
#'  Returns an array. \cr
#'  Allows for broadcasting.
#'  - `bind_dt()` binds data.tables and other data.frame-like objects. \cr
#'  Returns a `data.table`. \cr
#'  This function is only available if the 'data.table' package is installed. \cr
#'  The `bind_dt()` function is faster than `do.call(cbind, ...)` or `do.call(rbind, ...)` for regular `data.frame` objects. \cr
#' 
#' 
#' 
#' @param input a list of only the appropriate objects. \cr
#' If `input` is named,
#' its names will be used for the names of dimension `along` of the output,
#' as far as possible.
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc. \cr
#' For arrays, additional flexibility is available:
#'  * Specifying `along = 0` will bind the arrays on a new dimension before the first,
#'  making `along` the new first dimension.
#'  * Specifying `along = N + 1`, with `N = `\link[base]{max}`(`\link{lst.ndim}`(input))`,
#'  will create an additional dimension (`N + 1`) and bind the arrays along that new dimension.
#' @param rev Boolean, for `bind_array()` only. \cr
#' Indicates if `along` should be reversed, counting backwards. \cr
#' If `FALSE` (default), `along` works like normally; if `TRUE`, `along` is reversed. \cr
#' I.e. `along = 0, rev = TRUE` is equivalent to `along = N+1, rev = FALSE`; \cr
#' and `along = N+1, rev = TRUE` is equivalent to `along = 0, rev = FALSE`; \cr
#' with `N = `\link[base]{max}`(`\link{lst.ndim}`(input))`.
#' @param ndim2bc non-negative integer, for `bind_array` only. \cr
#' Specify here the maximum number of dimensions that are allowed to be broadcasted when binding arrays. \cr
#' If `ndim2bc = 0L`, \bold{no} broadcasting will be allowed at all.
#' @param name_deparse Boolean, for `bind_mat()`. \cr
#' Indicates if dimension `along` should be named. \cr
#' Uses the naming method from \link[base]{rbind}/\link[base]{cbind} itself.
#' @param name_along Boolean, for `bind_array()`. \cr
#' Indicates if dimension `along` should be named. \cr
#' The examples section illustrates the naming behaviour.
#' @param comnames_from either integer scalar or `NULL`,
#' for `bind_mat()` and  `bind_array()`. \cr
#' Indicates which object in `input` should be used for naming the shared dimension. \cr
#' If `NULL`, no communal names will be given. \cr
#' For example: \cr
#' When binding columns of matrices, the matrices will share the same rownames. \cr
#' Using `comnames_from = 10` will then result in `bind_array()` using
#' `rownames(input[[10]])` for the rownames of the output.
#' @param ... arguments to be passed to `data.table::rbindlist()`. \cr \cr
#' 
#' @details
#' For in-depth information about the binding implentations in the 'broadcast' package,
#' please refer to \link{broadcast_bind}. \cr
#' \cr
#'  
#' 
#' 
#' 
#' @returns
#' The bound object.
#'
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#'
#' @example inst/examples/bind.R
#' 
#'  


#' @name bind
NULL



#' @rdname bind
#' @export
bind_mat <- function(
    input, along, name_deparse = TRUE, comnames_from = 1L
) {
  
  # error checks:
  if(any(vapply(input, is.data.frame, logical(1L)))) {
    stop("use `bind_dt to bind data.frame-like objects")
  }
  if(any(lst.ndim(input) > 2L)) {
    stop("use `bind_array()` to bind arrays with more than 2 dimensions")
  }
  if(!.is.integer_scalar(along) || along < 0 || along > 2) {
    stop("`along` must be the integer scalar 1 or 2")
  }
  
  # fix input:
  input2 <- .bind_input_fix(input, FALSE, sys.call())
  
  # naming argument checks:
  .bind_stop_name_deparse(name_deparse, abortcall = sys.call())
  .bind_stop_comnames_from(comnames_from, input, abortcall = sys.call())
  
  # determine imargin:
  if(along == 1L) imargin <- 2L
  else if(along == 2L) imargin <- 1L
  
  # return original:
  if(length(input2) == 1L) {
    return(input2[[1L]])
  }
  
  # check for fractional recycling:
  sizes <- .rcpp_rcbind_get_sizes(input2, imargin - 1L)
  sizes <- sizes[sizes != 1L]
  if(length(sizes) > 1) {
    fractions <- sizes / min(sizes)
    if(any(fractions != round(fractions))) {
      stop("fractional recycling not allowed")
    }
  }
  
  # main function:
  name_deparse <- as.integer(name_deparse)
  if(along == 1L) {
    out <- do.call(rbind, c(input2, list(deparse.level = name_deparse)))
    not_along <- 2L
  }
  if(along == 2L) {
    out <- do.call(cbind, c(input2, list(deparse.level = name_deparse)))
    not_along <- 1L
  }
  
  # clear dimnames:
  if(name_deparse == 0L && is.null(comnames_from)) {
    dimnames(out) <- NULL
  }
  
  
  # remove alongnames if unneeded:
  if(name_deparse == 0L && !is.null(dimnames(out))) {
    out.dimnames <- dimnames(out)
    out.dimnames[along] <- list(NULL)
    dimnames(out) <- out.dimnames
  }
  
  
  # remove comnames to prep for next step:
  if(!is.null(dimnames(out))) {
    out.dimnames <- dimnames(out)
    out.dimnames[not_along] <- list(NULL)
    dimnames(out) <- out.dimnames
  }
  
  
  # recreate comnames:
  if(!is.null(comnames_from)) {
    comarg <- input[[comnames_from]]
    if(is.array(comarg) && !is.null(dimnames(comarg))) {
      dimnames(out)[[not_along]] <- dimnames(comarg)[[not_along]]
    }
    else {
      if(!is.null(names(comarg))) {
        dimnames(out)[[not_along]] <- names(comarg)
      }
    }
  }
  
  # return:
  return(out)
}


#' @rdname bind
#' @export
bind_array <- function(
    input, along, rev = FALSE, ndim2bc = 1L, name_along = TRUE, comnames_from = 1L
) {
  
  # error checks:
  all_arrays <- vapply(input, is.array, logical(1L)) |> all()
  if(!all_arrays) {
    stop(simpleError("can only bind arrays", call = sys.call()))
  }
  
  # input fix:
  input2 <- .bind_input_fix(input, FALSE, sys.call())
  
  
  # along fix:
  # check (rev)along:
  along <- .bind_arg_along(along, rev, max(lst.ndim(input2)), sys.call())
  
  
  # naming argument checks:
  .bind_stop_name_along(name_along, abortcall = sys.call())
  .bind_stop_comnames_from(comnames_from, input, abortcall = sys.call())
  
  
  # return original:
  if(length(input2) == 1L) {
    return(input2[[1L]])
  }

  # main function:
  out <- .internal_bind_array(input2, along, ndim2bc, name_along, sys.call())
  
  # add comnames:
  if(!is.null(comnames_from)) {
    out <- .bind_inplace_comnames(out, comnames_from, input, along) # using original input
  }
  
  # return output:
  return(out)
}


#' @rdname bind
#' @export
bind_dt <- function(
    input, along, ...
) {
  
  # data.table is a suggested package
  
  input2 <- .bind_input_fix(input, TRUE, sys.call())
  
  if(!.is.integer_scalar(along)) {
    stop("`along` must be the integer scalar 1 or 2")
  }
  if(along != 1L && along != 2L) {
    stop("`along` must be the integer scalar 1 or 2")
  }
  
  # return original:
  if(length(input2) == 1L) {
    return(input2[[1L]])
  }
  
  # main function:
  if(along == 1) {
    out <- data.table::rbindlist(input2, ...)
  }
  if(along == 2) {
    out <- do.call(data.table::data.table, c(input2, check.names = TRUE))
  }
  return(out)
  
}
