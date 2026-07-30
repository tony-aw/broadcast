#' Details on Casting Functions
#' 
#' @description
#' 'broadcast' provides several "casting" functions. \cr
#' These can facilitate complex forms of broadcasting that would normally not be possible. \cr
#' But these "casting" functions also have their own merit, beside empowering complex broadcasting. \cr
#' \cr
#' The following casting functions are available:
#' 
#'  - \link{acast}: \cr
#'  Casts group-based subsets of an array into a new dimension. \cr
#'  Useful for, for example, performing \bold{grouped} broadcasted operations.
#'  - \link{cast_hier2dim}: \cr
#'  Casts a nested/hierarchical list into a dimensional list (i.e. array of type `list`). \cr
#'  Useful because one cannot broadcast through nesting, but one \bold{can} broadcast along dimensions.
#'  - \link{hier2dim}, \link{hiernames2dimnames}: \cr
#'  Helper functions for \link{cast_hier2dim}.
#'  - \link{cast_dim2hier}: \cr
#'  Casts a dimensional list into a nested/hierarchical list; the opposite of \link{cast_hier2dim}.
#'  - \link{cast_shallow2atomic}: \cr
#'  Casts a (dimensional) shallow (i.e. non-nested) list into an atomic vector or array. \cr
#'  Useful because atomic vectors/arrays have access to many vectorized (broadcasted) operations that may not be available for vectors/arrays of type `list`.
#'  - \link{cast_dim2flat}: \cr
#'  Casts a dimensional list into a flattened list, but with names that indicate their original dimensional positions. \cr
#'  Mostly useful for printing or summarizing dimensional lists.
#'  - \link{dropnests}: \cr
#'  Drop redundant nesting in lists; mostly used for facilitating the above casting functions.
#'  - The \link[=reorient_vector]{reorient_vector} (`%orientbc<-%`) operator: \cr
#'  Changes the orientation of an oriented vector. \cr
#'  Oriented vectors can be used to modify an array along a dimension. \cr \cr
#' 
#' @section Shared argument `recurse_all`:
#' The \link{dropnests}, \link{hier2dim}, \link{hiernames2dimnames}, and \link{cast_hier2dim} methods
#' all have the `recurse_all` argument. \cr
#' By default `recurse_all = FALSE`,
#' meaning these methods do not recurse through dimensional or classed lists (like data.frames). \cr
#' Setting `recurse_all = TRUE`
#' allows these methods to recurse through all list objects,
#' even if they are dimensional and/or classed. \cr
#' \cr
#' 
#' 
#' @section Shared Argument `in2out`: 
#' The \link{hier2dim}, \link{hiernames2dimnames}, \link{cast_hier2dim}, and \link{cast_dim2hier} methods
#' all have the `in2out` argument. \cr
#' \cr
#' \cr
#' `r .mybadge_casting("in2out", TRUE, "green")` \cr
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
#' `y <- cast_hier2dim(x)` \cr
#' `x <- cast_dim2hier(y)`. \cr
#' Then it holds that: \cr
#' `x[[i]][[j]][[k]]` corresponds to `y[[k, j, i]]`, \cr
#' \eqn{\forall}(`i`, `j`, `k`) , provided `x[[i]][[j]][[k]]` exists. \cr
#' \cr
#' \cr
#' `r .mybadge_casting("in2out", FALSE, "red")` \cr
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
#' `y <- cast_hier2dim(x, in2out = FALSE)` \cr
#' `x <- cast_dim2hier(y, in2out = FALSE)`. \cr
#' Then it holds that : \cr
#' `x[[i]][[j]][[k]]` corresponds to `y[[i, j, k]]`, \cr
#' \eqn{\forall}(`i`, `j`, `k`) , provided `x[[i]][[j]][[k]]` exists. \cr \cr
#' 
#' @example inst/examples/broadcast_cast.R
#' 
#' @name aaa02_broadcast_casting
#' @rdname aaa02_broadcast_casting
#' @aliases broadcast_casting
#' 
NULL
#> NULL
