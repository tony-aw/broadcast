#' Dimensional Binding of Arrays with Broadcasting
#'
#' @description
#' `bind_array()` binds (atomic/recursive) arrays and (atomic/recursive) matrices. \cr
#' Returns an array. \cr
#' Allows for broadcasting.
#' 
#' 
#' @param input a list of arrays; both atomic and recursive arrays are supported, and can be mixed. \cr
#' If `input` is named,
#' its names will be used for the names of dimension `along` of the output, when necessary. \cr
#' If argument `input` has length `0`,
#' or it contains exclusively objects where one or more dimensions are `0`,
#' an error is returned. \cr
#' If `input` has length `1`, this function simply return `input[[1L]]`.
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc. \cr
#' Specifying `along = 0` will bind the arrays on a new dimension before the first,
#' making `along` the new first dimension. \cr
#' Specifying `along = N + 1`, with `N = max(`\link{lst.ndim}`(input))`,
#' will create an additional dimension (`N + 1`) and bind the arrays along that new dimension.
#' @param rev Boolean, indicating if `along` should be reversed, counting backwards. \cr
#' If `FALSE` (default), `along` works like normally; if `TRUE`, `along` is reversed. \cr
#' I.e. `along = 0, rev = TRUE` is equivalent to `along = N+1, rev = FALSE`; \cr
#' and `along = N+1, rev = TRUE` is equivalent to `along = 0, rev = FALSE`; \cr
#' with `N = max(`\link{lst.ndim}`(input))`.
#' @param ndim2bc a single non-negative integer; \cr
#' specify here the maximum number of dimensions that are allowed to be broadcasted when binding arrays. \cr
#' If `ndim2bc = 0L`, \bold{no} broadcasting will be allowed at all.
#' @param name_along Boolean, ndicating if dimension `along` should be named. \cr
#' The examples section illustrates the naming behaviour.
#' @param comnames_from either integer scalar or `NULL`. \cr
#' Indicates which object in `input` should be used for naming the shared dimension. \cr
#' If `NULL`, no communal names will be given. \cr
#' For example: \cr
#' When binding columns of matrices, the matrices will share the same rownames. \cr
#' Using `comnames_from = 10` will then result in `bind_array()` using
#' `rownames(input[[10]])` for the rownames of the output. \cr \cr
#' 
#' 
#' @details
#' The API of `bind_array()` is inspired by the fantastic
#' \code{abind::abind} function
#' by Tony Plare & Richard Heiberger (2016). \cr
#' But `bind_array()` differs considerably from \code{abind::abind}
#' in the following ways:
#'  
#'  - `bind_array()` differs from \code{abind::abind}
#'  in that it can handle recursive arrays properly \cr
#'  (the \code{abind::abind} function would unlist everything to atomic arrays,
#'  ruining the structure).
#'  - `bind_array()` allows for broadcasting,
#'  while \code{abind::abind} does not support broadcasting.
#'  - `bind_array()` is generally faster than \code{abind::abind},
#'  as `bind_array()` relies heavily on 'C' and 'C++' code.
#'  - unlike \code{abind::abind},
#'  `bind_array()` only binds (atomic/recursive) arrays and matrices. \cr
#'  `bind_array()`does not attempt to convert things to arrays when they are not arrays,
#'  but will give an error instead. \cr
#'  This saves computation time and prevents unexpected results.
#'  - `bind_array()` has more streamlined naming options,
#'  compared to \code{abind::abind}. \cr \cr
#'  
#' 
#' 
#' 
#' @returns
#' An array.
#'
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#'
#' @example inst/examples/bind_array.R
#' 
#'  


#' @rdname bind_array
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

