#' Turn Vector to Array and Vice-Versa
#'
#' @description
#' `vector2array()` turns a vector into an array,
#' with a specific vector direction,
#' and turning the names into dimnames, and keeping (or forcing) \link{broadcaster} attribute. \cr
#' \cr
#' `undim()` returns a copy of an object, but with its dimensions removed,
#' but still trying to keep the names if possible
#' (it somewhat is like the dimensional version of `unlist()`). \cr
#' `undim()` will also keep (or force) the \link{broadcaster} attribute
#' \cr
#' `array2vector()` is an alias for `undim()`. \cr \cr
#' 
#' @param x an vector (for `vector2array()` or an array (for `undim()`/`array2vector()`). \cr
#' All atomic types, and the recursive type `list`, are supported.
#' @param direction a positive integer scalar, giving the direction of the vector. \cr
#' In other words: give here which dimension should have size `length(x)` - all other dimensions will have size `1`.
#' @param ndim the number of dimensions in total. \cr
#' It must be the case that `ndim >= direction`, and `ndim <= 16L`.
#' @param broadcaster `TRUE` or `FALSE`, indicating if the result should be a broadcaster. \cr
#' If `NULL`, `broadcaster(x)` will be used. \cr
#'
#'
#' @returns
#' For `vector2array()`: \cr
#' If `x` is already an array, `x` is returned unchanged. \cr
#' Otherwise, given `out <- vector2array(x, direction, ndim)`,
#' `out` will be an array with the following properties:
#' 
#'  - `ndim(out) == ndim`;
#'  - `dim(out)[direction] == length(x)`, and all other dimensions will be `1`;
#'  - `dimnames(out)[[direction]] == names(x)`, and all other `dimnames` will be `NULL`. \cr \cr
#' 
#' For `undim()`: \cr
#' If `x` is not an array, `x` is returned unchanged. \cr
#' Otherwise, a copy of the original object, but without dimensions,
#' but keeping names and \link{broadcaster} attribute as far as possible. \cr \cr
#'
#'
#' @example inst/examples/vector2array.R
#' 


#' @name vector2array
NULL


#' @rdname vector2array
#' @export
vector2array <- function(x, direction, ndim = direction, broadcaster = NULL) {
  
  
  # checks:
  if(!.is.integer_scalar(direction) || direction < 1L || direction > 16L) {
    stop("`direction` must be a strictly positive integer scalar")
  }
  if(!.is.integer_scalar(ndim) || ndim < direction || ndim > 16) {
    stop("`ndim` must be a strictly positive integer scalar, and `>= direction` and `<= 16`")
  }
  if(!is.atomic(x) && !.is_list(x)) {
    stop("`x` must be atomic or a list")
  }
  if(is.null(broadcaster)) {
    broadcaster <- broadcaster(x)
  }
  if(!isTRUE(broadcaster) && !isFALSE(broadcaster)) {
    stop("`broadcaster` must be `TRUE` or `FALSE`")
  }
  
  # quick return:
  if(is.array(x)) {
    return(x)
  }
  
  # get params:
  out.dim <- rep(1L, ndim)
  out.dim[direction] <- length(x)
  if(!is.null(names(x))) {
    out.dimnames <- rep(list(NULL), ndim)
    out.dimnames[[direction]] <- names(x)
  }
  else {
    out.dimnames <- NULL
  }
  
  # make out:
  out <- x # automatically keep all relevant attributes
  dim(out) <- out.dim
  dimnames(out) <- out.dimnames
  broadcaster(out) <- broadcaster
  
  return(out)
}

#' @rdname vector2array
#' @export
undim <- function(x, broadcaster = NULL) {
  
  # checks:
  if(!is.atomic(x) && !.is_list(x)) {
    stop("`x` must be atomic or a list")
  }
  if(is.null(broadcaster)) {
    broadcaster <- broadcaster(x)
  }
  if(!isTRUE(broadcaster) && !isFALSE(broadcaster)) {
    stop("`broadcaster` must be `TRUE` or `FALSE`")
  }
  
  # quick return:
  if(!is.array(x)) {
    return(x)
  }
  
  # get params:
  if(length(x) == 0L) {
    out.names <- NULL
  }
  else if(!is.null(dimnames(x))) {
    x.dimnames <- dimnames(x)
    ind <- lengths(x.dimnames) == length(x)
    if(any(ind)) {
      ind <- which(ind)[1L]
      out.names <- x.dimnames[[ind]]
    }
  }
  else {
    out.names <- names(x)
  }
  
  
  # make out:
  out <- x # automatically keep all relevant attributes
  dim(out) <- NULL
  names(out) <- out.names
  broadcaster(out) <- broadcaster
  
  return(out)
}


#' @rdname vector2array
#' @export
array2vector <- undim
