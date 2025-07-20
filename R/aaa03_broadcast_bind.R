#' Dimensional Binding of Arrays with Broadcasting
#'
#' @description
#' `bind_array()` binds (atomic/recursive) arrays and (atomic/recursive) matrices. \cr
#' Allows for broadcasting. \cr \cr
#' 
#' 
#' @param input a list of arrays; both atomic and recursive arrays are supported, and can be mixed. \cr
#' If argument `input` has length `0`,
#' or it contains exclusively objects where one or more dimensions are `0`,
#' an error is returned. \cr
#' If `input` has length `1`, `bind_array()` simply returns `input[[1L]]`. \cr
#' `input` may not contain more than `2^16` objects.
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc. \cr
#' Specifying `along = 0` will bind the arrays on a new dimension before the first,
#' making `along` the new first dimension. \cr
#' Specifying `along = N + 1`, with `N = max(lst.ndim(input))`,
#' will create an additional dimension (`N + 1`) and bind the arrays along that new dimension.
#' @param rev Boolean, indicating if `along` should be reversed, counting backwards. \cr
#' If `FALSE` (default), `along` works like normally; if `TRUE`, `along` is reversed. \cr
#' I.e. `along = 0, rev = TRUE` is equivalent to `along = N+1, rev = FALSE`; \cr
#' and `along = N+1, rev = TRUE` is equivalent to `along = 0, rev = FALSE`; \cr
#' with `N = max(lst.ndim(input))`.
#' @param ndim2bc a single non-negative integer; \cr
#' specify here the maximum number of dimensions that are allowed to be broadcasted when binding arrays. \cr
#' If `ndim2bc = 0L`, \bold{no} broadcasting will be allowed at all.
#' @param name_along Boolean, indicating if dimension `along` should be named. \cr
#' Please run the code in the examples section to get a demonstration of the naming behaviour.
#' @param comnames_from either an integer scalar or `NULL`. \cr
#' Indicates which object in `input` should be used for naming the shared dimension. \cr
#' If `NULL`, no communal names will be given. \cr
#' For example: \cr
#' When binding columns of matrices, the matrices will share the same rownames. \cr
#' Using `comnames_from = 10` will then result in `bind_array()` using
#' `rownames(input[[10]])` for the rownames of the output. \cr \cr
#' 
#' 
#' 
#' @returns
#' An array. \cr  \cr
#'
#'
#' @example inst/examples/bind_array.R
#' 
#'  

#' @name bind_array
#' @rdname bind_array
#' @export
NULL
