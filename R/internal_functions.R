#

#' @keywords internal
#' @noRd
.stop_general <- function(x, y, abortcall) {
  if(!is.atomic(x) || !is.atomic(y) || !is.array(x) || !is.array(y)) {
    stop(simpleError("`x` and `y` must both be atomic arrays", call = abortcall))
  }
  if(.ndims(x) > 16L || .ndims(y) > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.stop_conf_dim <- function(x, y, abortcall) {
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  if(is.null(x.dim) || is.null(y.dim)) {
    if(length(x) != length(y)) {
      if(length(x) != 1 && length(y) != 1) {
        stop(simpleError("`x` and `y` are not conformable", call = abortcall))
      }
    }
  }
  else {
    out <- .C_check_conf_dim(dim(x), dim(y))
    if(!out) {
      stop(simpleError("`x` and `y` are not conformable", call = abortcall))
    }
  }
}


#' @keywords internal
#' @noRd
.determine_dimmode <- function(x.dim, y.dim, out.dim, abortcall) {
  
  # use vector mode:
  if(length(x.dim) == 1L || length(y.dim) == 1L) { # x and/or y are/is scalar(s)
    return(1L)
  }
  else if(is.null(x.dim) || is.null(y.dim)) { # x and/or y are/is vector(s)
    return(1L)
  }
  else if(all(x.dim == y.dim)) { # x & y have same dimensions, thus same ordering as output
    return(1L)
  }
  
  # use orthogonal mode:
  if(all(x.dim != y.dim)) {
    return(2L)
  }
  
  # use one-off mode (i.e. one of the arrays does not need to be broadcasted):
  if(all(x.dim == out.dim) || all(y.dim == out.dim)) {
    return(3L)
  }
  
  # use one-common mode (i.e. for one dimension flat indices does not need to be calculated):
  if(sum(x.dim == y.dim) == 1L) {
    return(4L)
  }
  
  # use miscellaneous mode :
  if(length(x.dim) <= 8L && length(y.dim) <= 8L) { # array result with <= 8 dims
    return(5L)
  }
  else { # larger array result
    return(6L)
  }
  
}


#' @keywords internal
#' @noRd
.determine_out.dim <- function(x.dim, y.dim) {
  if(is.null(x.dim) && is.null(y.dim)) {
    return(NULL)
  }
  else if(!is.null(x.dim) && !is.null(y.dim)) {
    return(.C_pmax(x.dim, y.dim))
  }
  else if(!is.null(x.dim)) {
    return(x.dim)
  }
  else {
    return(y.dim)
  }
}


#' @keywords internal
#' @noRd
.determine_out.len <- function(x, y, out.dim) {
  if(is.null(dim(x)) || is.null(dim(y))) {
    return(max(length(x), length(y)))
  }
  else {
    return(prod(out.dim))
  }
  
}


#' @keywords internal
#' @noRd
.make_by <- function(target.dim, out.dim) {
  ind1 <- which(target.dim == 1L)
  ind0 <- which(target.dim > 1L)
  by <- vector("integer", length(target.dim))
  by[ind1] <- 0L
  by[ind0] <- 1L
  return(by)
}


#' @keywords internal
#' @noRd
.make_indices <- function(target.dim, out.dim) {
  ind1 <- which(target.dim == 1L)
  ind0 <- which(target.dim > 1L)
  indices <- vector("list", length(target.dim))
  indices[ind1] <- lapply(out.dim[ind1], \(n)rep(1L, n))
  indices[ind0] <- lapply(out.dim[ind0], \(n)1:n)
  return(indices)
}


#' @keywords internal
#' @noRd
.make_dcp <- function(dims) {
  return(c(1, cumprod(dims)))
}


#' @keywords internal
#' @noRd
.return_NA <- function(x) {
  if(is.logical(x)) {
    return(rep(NA, length(x)))
  }
  else if(is.integer(x)) {
    return(rep(NA_integer_, length(x)))
  }
  else if(is.double(x)) {
    return(rep(NA_real_, length(x)))
  }
  else if(is.complex(x)) {
    return(rep(NA_complex_, length(x)))
  }
  else if(is.character(x)) {
    return(rep(NA_character_, length(x)))
  }
}


#' @keywords internal
#' @noRd
.op_dbl <- function(op, abortcall) {
  op <- trimws(op, which = "both")
  if(op == "+") {
    return(1L)
  }
  else if(op == "-") {
    return(2L)
  }
  else if(op == "*") {
    return(3L)
  }
  else if(op == "/") {
    return(4L)
  }
  else if(op == "^"){
    return(5L)
  }
  else if(op == "pmin") {
    return(6L)
  }
  else if(op == "pmax") {
    return(7L)
  }
  else {
    stop(simpleError("given operator not supported in the given context"))
  }
}


#' @keywords internal
#' @noRd
.ndims <- function(x) {
  return(length(dim(x)))
}


#' @keywords internal
#' @noRd
.prep <- function(x, y) {
  
  # drop dimensions for scalars:
  if(length(x) == 1L) {
    x <- drop(x)
  }
  if(length(y) == 1L) {
    y <- drop(y)
  }
  
  # drop dimension when both x and y are strictly 1d arrays or vectors:
  if(.ndims(x) <= 1L && .ndims(y) <= 1L) {
    # if both are 1d arrays or vectors, drop dimensions
    # if only one is a 1d array, DON'T drop dimensions,
    # since it may be orthogonal broadcasting
    # (i.e. colvector * rowvector = 1d * 2d)
    # also, 1d %op% matrix is not the same as vector %op% matrix when broadcasted
    dim(x) <- NULL
    dim(y) <- NULL
  }
  
  # normalize dimensions:
  x.dim <- dim(x)
  y.dim <- dim(y)
  if(!is.null(x.dim) && !is.null(y.dim)) {
    x.ndims <- length(x.dim)
    y.ndims <- length(y.dim)
    if(x.ndims > y.ndims) {
      dim(y) <- c(y.dim, rep(1L, x.ndims - y.ndims))
    }
    if(y.ndims > x.ndims) {
      dim(x) <- c(x.dim, rep(1L, y.ndims - x.ndims))
    }
  }
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  # 
  # # drop common 1L dimensions, if both x and y have more than 1 dimension:
  # if(length(x.dim) > 1L && length(y.dim) > 1L) {
  #   ind.drop <- which((x.dim == 1L) & (y.dim == 1L))
  #   if(length(ind.drop) > 0L) {
  #     x.dim <- x.dim[-ind.drop]
  #     y.dim <- y.dim[-ind.drop]
  #   }
  #   if(length(x.dim) == 0L) x.dim <- NULL
  #   if(length(y.dim) == 0L) y.dim <- NULL
  #   dim(x) <- x.dim
  #   dim(y) <- y.dim
  # }
  
  return(list(x, y))
  
}
