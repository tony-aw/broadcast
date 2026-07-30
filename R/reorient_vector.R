#' Change Orientation of An Oriented Vector and Make it a Broadcaster
#'
#' @description
#' The `lhs %orientbc<-% rhs` operator
#' takes an oriented vector (`lhs`),
#' and changes its orientation. \cr
#' It also makes `lhs` a broadcaster if it wasn't already. \cr
#' \cr
#' The modification is done directly on `lhs` itself,
#' using R's native semantics. \cr
#' \cr
#' 
#' 
#' @param lhs an atomic or recursive oriented vector.
#' @param rhs an integer vector. \cr
#' The first value must be the new orientation,
#' referred to as `orient`. \cr
#' The second value, optional,
#' must be the number of dimensions, referred to as `ndim`. \cr
#' If no second value is given, it will be set at `max(ndim(lhs), rhs[1])`. \cr
#' \cr
#' 
#' @details
#' In the context of broadcasting,
#' an oriented vector `x` is a vector or array that satisfies the following conditions:
#' 
#'  - It has length `> 1` and `<= (2^31 - 1)`;
#'  - It has no dimensions,
#'  or one dimension is `length(x)` and all other dimensions are `1`. \cr
#' 
#' The orientation of an oriented vector is the index of the dimension with size > 1. \cr
#' For example, if array `lhs` has dimensions `c(1, 10, 1)`,
#' its orientation is `2L`,
#' since the second dimension has size > 1. \cr
#' A vector with no dimensions has orientation `1L`. \cr
#' \cr
#' In the context of broadcasted operations, 
#' oriented vectors can be used to modify an array along a dimension. \cr
#' The examples section gives several examples of this. \cr
#' \cr
#' 
#' 
#' @returns
#' Nothing, but changes `lhs` directly. \cr
#' \cr
#'
#' @seealso \link{broadcast_casting} \cr
#' \cr
#'
#' @example inst/examples/reorient_vector.R
#' 
#' @name reorient_vector
NULL

#' @rdname reorient_vector
#' @export
`%orientbc<-%` <- function(lhs, rhs) {
  
  
  # prep rhs:
  .reorient_vector_check_rhs(rhs, sys.call())
  if(length(rhs) == 1L) {
    orient <- rhs[1L]
    ndim <- .missing()
  }
  if(length(rhs) == 2L) {
    orient <- rhs[1L]
    ndim <- rhs[2L]
  }
  
  # prep lhs:
  x_expr <- substitute(lhs)
  env <- parent.frame()

  # call replacement function:
  .reorient_vector_replace(x_expr, orient, ndim, env, sys.call())
  
}


#' @keywords internal
#' @noRd
.reorient_vector_replace <- function(x_expr, orient, ndim, env, abortcall) {
  
  # get properties:
  meta_expr <- substitute(
    list(
      type      = base::typeof(ARR),
      len       = base::length(ARR),
      dim       = base::dim(ARR),
      names     = base::names(ARR),
      dimnames  = base::dimnames(ARR),
      class     = base::oldClass(ARR)
    ), 
    list(ARR = x_expr)
  )
  x_meta <- eval(meta_expr, envir = env)
  
  main <- .reorient_vector_main(x_meta, orient, ndim, abortcall)
  new.dim <- main$dim
  new.dimnames <- main$dimnames
  
  subs <- list(
    x = x_expr,
    new.dim = new.dim,
    new.dimnames = new.dimnames,
    `<-` = `<-`
  )
  expr <- substitute(
    {
      base::dim(x) <- new.dim
      base::dimnames(x) <- new.dimnames
    }, subs
  )
  eval(expr, envir = env)
  mbroadcasters(as.character(x_expr), TRUE, env)
  
  
}

#' @keywords internal
#' @noRd
.reorient_vector_main <- function(x_meta, orient, ndim, abortcall) {
  x.len       <- x_meta$len
  x.dim       <- x_meta$dim
  x.names     <- x_meta$names
  x.dimnames  <- x_meta$dimnames
  x.class     <- x_meta$class
  x.ndim      <- length(x.dim)
  
  # check lhs:
  .reorient_vector_check_lhs(x.len, x.dim, x.class, abortcall)
  
  
  # make (new) orient, dim, and ndim:
  if(x.ndim <= 1L) {
    x.orient <- 1L
    if(.is.missing(ndim)) {
      ndim <- orient
    }
  }
  else {
    x.orient <- which(x.dim > 1L)
    if(.is.missing(ndim)) {
      ndim <- max(length(x.dim), orient)
    }
  }
  
  # MAIN FUNCTION:
  
  new.dim <- rep(1L, ndim)
  new.dim[orient] <- x.len
  new.dimnames <- .reorient_vector_names(
    ndim, orient, x.dim, x.ndim, x.names, x.dimnames, x.orient
  )
  
  out <- list(
    dim = new.dim,
    dimnames = new.dimnames
  )
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.reorient_vector_names <- function(ndim, orient, x.dim, x.ndim, x.names, x.dimnames, x.orient) {
  if(is.null(x.names) && is.null(x.dimnames)) {
    return(NULL)
  }
  if(x.ndim <= 1L && !is.null(x.names)) {
    new.dimnames <- rep(list(NULL), ndim)
    new.dimnames[[orient]] <- x.names
    return(new.dimnames)
  }
  if(x.ndim > 1L && !is.null(x.dimnames)) {
    new.dimnames <- rep(list(NULL), ndim)
    new.dimnames[[orient]] <- x.dimnames[[x.orient]]
    return(new.dimnames)
  }
  return(NULL)
}

#' @keywords internal
#' @noRd
.reorient_vector_check_lhs <- function(x.len, x.dim, x.class, abortcall) {
  
  # check x:
  if(isTRUE("data.frame" %in% x.class)) {
    stop(simpleError("`lhs` must be a vector or array", call = abortcall))
  }
  if(x.len <= 1L || x.len > (2^31 -1)) {
    stop(simpleError("unsupported length of `lhs`", call = abortcall))
  }
  if(sum(x.dim > 1L) > 1L) {
    stop(simpleError("`lhs` is a multi-dimensional array", call = abortcall))
  }
  if(length(x.dim) > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported",
                     call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.reorient_vector_check_rhs <- function(rhs, abortcall) {
  
  if(!is.numeric(rhs) || (!length(rhs) %in% 1:2) || anyNA(rhs)) {
    stop(simpleError(
      "`rhs` must be a integer vector of length 1 or 2 without missing values",
      call = abortcall
    ))
  }
  
  if(any(rhs != round(rhs))) {
    stop(simpleError("`rhs` must consist of only whole numbers", call = abortcall))
  }
  
  if(any(rhs > 16L) || any(rhs < 1)) {
    stop(simpleError("`rhs` may not be larger than 16 or smaller than 1"))
  }
  
  if(length(rhs) == 2L && (rhs[1] > rhs[2])) {
    stop(simpleError(
      "the orientation (`rhs[1]`) cannot be larger than the number of dimensions (`rhs[2]`)",
      call = abortcall
    ))
  }
  
  
}


#' @keywords internal
#' @noRd
.missing <- function() {
  return(structure(list(), class = "missing"))
}


#' @keywords internal
#' @noRd
.is.missing <- function(x) {
  return(is.list(x) && class(x) == "missing")
}




