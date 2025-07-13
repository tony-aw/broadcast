#' Details on Casting Functions
#' 
#' @description
#' This help page gives some additional information on the casting functions
#' provided by the 'broadcast' package. \cr
#' \cr
#' 
#' 
#' @section Argument `in2out = TRUE`: 
#' The \link{hier2dim}, \link{cast_hier2dim}, and \link{cast_dim2hier} methods
#' all have the `in2out` argument. \cr
#' By default `in2out` is `TRUE`. \cr
#' This means the call \cr
#' `y <- cast_hier2dim(x)` \cr
#' will cast the elements of the deepest valid depth of `x` to the rows of `y`,
#' and elements of the depth above that to the columns of `y`,
#' and so on until the surface-level elements of `x` are cast to the last dimension of `y`. \cr
#' \cr
#' Similarly, the call \cr
#' `x <- cast_dim2hier(y)` \cr
#' will cast the rows of `y` to the inner most elements of `x`,
#' and the columns of `y` to one depth above that,
#' and so on until the last dimension of `y` is cast to the surface-level elements of `x`. \cr
#' \cr
#' Consider the nested list `x` with a depth of `3`,
#' and the recursive array `y` with 3 dimensions,
#' where their relationship can described as the following code: \cr
#' `x <- cast_hier2dim(y)` \cr
#' `y <- cast_dim2hier(x)`. \cr
#' Then it holds that: \cr
#' `x[[i]][[j]][[k]]` corresponds to `y[[k, j, i]]`, \cr
#' \eqn{\forall}(`i`, `j`, `k`) , provided `x[[i]][[j]][[k]]` exists. \cr \cr
#' 
#' 
#' 
#' @section Argument `in2out = FALSE`:
#' The \link{hier2dim}, \link{cast_hier2dim}, and \link{cast_dim2hier} methods
#' all have the `in2out` argument. \cr
#' If `in2out = FALSE`, the call \cr
#' `y <- cast_hier2dim(x, in2out = FALSE)` \cr
#' will cast the surface-level elements of `x` to the rows of `y`,
#' and elements of the depth below that to the columns of `y`,
#' and so on until the elements of the deepest valid depth of `x` are cast to the last dimension of `y`. \cr
#' \cr
#' Similarly, the call \cr
#' `x <- cast_dim2hier(y, in2out = FALSE)` \cr
#' will cast the rows of `y` to the surface-level elements of `x`,
#' and the columns of `y` to one depth below that,
#' and so on until the last dimension of `y` is cast to the inner most elements of `x`. \cr
#' \cr
#' Consider the nested list `x` with a depth of `3`,
#' and the recursive array `y` with 3 dimensions,
#' where their relationship can described with the following code: \cr
#' `x <- cast_hier2dim(y, in2out = FALSE)` \cr
#' `y <- cast_dim2hier(x, in2out = FALSE)`. \cr
#' Then it holds that : \cr
#' `x[[i]][[j]][[k]]` corresponds to `y[[i, j, k]]`, \cr
#' \eqn{\forall}(`i`, `j`, `k`) , provided `x[[i]][[j]][[k]]` exists. \cr \cr
#' 
#' 
#' @example inst/examples/aaa_broadcast_casting.R
#' 
#' @name aaa02_broadcast_casting
#' @rdname aaa02_broadcast_casting
#' @aliases broadcast_casting
#' 
NULL
#> NULL
