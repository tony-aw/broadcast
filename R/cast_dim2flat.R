#' Cast Dimensional List into a Flattened List
#'
#' @description
#' `cast_dim2flat()` casts a dimensional list
#' (i.e. recursive array)
#' into a flat list
#' (i.e. recursive vector),
#' but with names that indicate the original dimensional positions of the elements. \cr
#' \cr
#' Primary purpose for this function
#' is to facilitate printing or summarizing dimensional lists. \cr
#' \cr
#' 
#' 
#' @param x a list
#' @param ... further arguments passed to or from methods. \cr \cr
#' 
#' 
#' 
#' @returns
#' A flattened list,
#' with names that indicate the original dimensional positions of the elements. \cr
#' \cr
#' 
#'
#' @seealso \link{broadcast_casting} \cr
#' @example inst/examples/cast_hier2dim.R
#' 
#'
#'

#' @rdname cast_dim2flat
#' @export
cast_dim2flat <- function(x, ...) {
  UseMethod("cast_dim2flat", x)
}

#' @rdname cast_dim2flat
#' @export
cast_dim2flat.default <- function(x, ...) {
  
  .hiercast_check_dims(x, sys.call())
  
  x.dimnames <- dimnames(x)
  x.dim <- dim(x)
  x.ndim <- ndim(x)
  x.len <- length(x)
  if(is.null(x.dimnames)) {
    dcp <- .C_make_dcp(x.dim)
    dimnumbers <- vector("list", x.ndim)
    dimnumbers[[1]] <- rep_len(1:x.dim[1], x.len)
    for(i in 2:x.ndim) {
      temp <- rep(1:x.dim[i], each = dcp[i])
      temp <- rep_len(temp, x.len)
      dimnumbers[[i]] <- temp
    }
    flatnames <- do.call(paste, c(dimnumbers, list(sep = ", ")))
    flatnames <- paste0("[", flatnames, "]")
  }
  else {
    dcp <- .C_make_dcp(x.dim)
    out.dimnames <- vector("list", x.ndim)
    for(i in 1:x.ndim) {
      if(is.null(x.dimnames[[i]])) {
        temp <- rep(1:x.dim[i], each = dcp[i])
        temp <- rep_len(temp, x.len)
      }
      else {
        temp <- paste("'", x.dimnames[[i]], "'", sep = "")
        temp <- rep(temp, each = dcp[i])
        temp <- rep_len(temp, x.len)
      }
      out.dimnames[[i]] <- temp
    }
    flatnames <- do.call(paste, c(out.dimnames, list(sep = ", ")))
    flatnames <- paste0("[", flatnames, "]")
  }
  dim(x) <- NULL
  names(x) <- flatnames
  return(x)
}

