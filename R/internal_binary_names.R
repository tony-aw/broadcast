
#' @keywords internal
#' @noRd
.binames_consider_dim <- function(target.dimnames, target.dim, out.dim) {
  res <- !is.null(target.dimnames) &&
    .C_binames_consider_dim(out.dim, target.dim, target.dimnames)
  return(res)
}


#' @keywords internal
#' @noRd
.binames_consider_flat <- function(target, out.len) {
  res <- length(target) == out.len && !is.null(names(target))
  return(res)
}


#' @keywords internal
#' @noRd
.binames_set <- function(x, y, out) {
  if(length(out) == 0L) {
    return(invisible(NULL))
  }
  
  out.dim <- dim(out)
  
  if(is.null(out.dim)) {
    .binames_set_flat(x, y, out)
    return(invisible(NULL))
  }
  else if(length(out.dim) == 1L) {
    .binames_set_1d(x, y, out)
  }
  else {
    .binames_set_dim(x, y, out, out.dim)
    return(invisible(NULL))
  }
}



#' @keywords internal
#' @noRd
.binames_set_dim <- function(x, y, out, out.dim) {
  
  # QUICK EJECTION:
  x.names <- names(x)
  x.dimnames <- dimnames(x)
  y.names <- names(y)
  y.dimnames <- dimnames(y)
  ejx <- is.null(x.names) && is.null(x.dimnames)
  ejy <- is.null(y.names) && is.null(y.dimnames)
  ej <-  ejx && ejy
  if(ej) {
    return(invisible(NULL))
  }
  
  
  # PREP:
  checkx <- !ejx
  checky <- !ejy
  if(checkx) {
    if(is.null(dim(x))) {
      x.dim <- length(x)
      x.dimnames <- list(x.names)
    }
    else {
      x.dim <- dim(x)
      x.dimnames <- x.dimnames
    }
    checkx <- .binames_consider_dim(x.dimnames, x.dim, out.dim)
  }
  if(checky) {
    if(is.null(dim(y))) {
      y.dim <- length(y)
      y.dimnames <- list(y.names)
    }
    else {
      y.dim <- dim(y)
      y.dimnames <- y.dimnames
    }
    checky <- .binames_consider_dim(y.dimnames, y.dim, out.dim)
  }
  
  
  # CONTINUE MAIN FUNCTION:
  if(!checkx && !checky) {
    return(invisible(NULL))
  }
  
  if(checkx && checky) {
    # consider both `x` and `y`
    x.ndim <- length(x.dim)
    y.ndim <- length(y.dim)
    x.len <- length(x)
    y.len <- length(y)
    pref <- 0L
    if(x.len > y.len && x.ndim > y.ndim) pref <- 1L
    if(y.len > x.len && y.ndim > x.ndim) pref <- 2L
    out.dimnames <- .rcpp_make_dimnames2(x.dimnames, y.dimnames, out.dim, pref)
    .rcpp_set_attr(out, "dimnames", out.dimnames)
    return(invisible(NULL))
    
  } # end consider both `x` and `y`
  
  
  if(checkx) {
    # consider only `x`
    out.dimnames <- .rcpp_make_dimnames1(x.dimnames, x.dim, out.dim)
    .rcpp_set_attr(out, "dimnames", out.dimnames)
    return(invisible(NULL))
  } # end consider only `x`
  
  
  if(checky) {
    # consider only `y`
    out.dimnames <- .rcpp_make_dimnames1(y.dimnames, y.dim, out.dim)
    .rcpp_set_attr(out, "dimnames", out.dimnames)
    return(invisible(NULL))
  } # end consider only `y`
  
  
  # else:
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.binames_set_1d <- function(x, y, out) {
  out.len <- length(out)
  
  checkx <- .binames_consider_flat(x, out.len)
  checky <- .binames_consider_flat(y, out.len)
  
  if(!checkx && !checky) {
    return(invisible(NULL))
  }
  
  if(checkx && checky) {
    # `x` and `y` conflict; only use if their names point to same vector
    if(.rcpp_address(names(x)) == .rcpp_address(names(y))) {
      .rcpp_set_attr(out, "dimnames", list(names(x)))
      return(invisible(NULL))
    }
    else {
      return(invisible(NULL))
    }
    
  }
  
  if(checkx) { # use `x`
    .rcpp_set_attr(out, "dimnames", list(names(x)))
    return(invisible(NULL))
  }
  
  
  if(checky) { # use `y`
    .rcpp_set_attr(out, "dimnames", list(names(y)))
    return(invisible(NULL))
  }
  
  
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.binames_set_flat <- function(x, y, out) {
  out.len <- length(out)
  
  checkx <- .binames_consider_flat(x, out.len)
  checky <- .binames_consider_flat(y, out.len)
  
  if(!checkx && !checky) {
    return(invisible(NULL))
  }
  
  if(checkx && checky) {
    # `x` and `y` conflict; only use if their names point to same vector
    if(.rcpp_address(names(x)) == .rcpp_address(names(y))) {
      .rcpp_set_attr(out, "names", names(x))
      return(invisible(NULL))
    }
    else {
      return(invisible(NULL))
    }
    
  }
  
  if(checkx) { # use `x`
    .rcpp_set_attr(out, "names", names(x))
    return(invisible(NULL))
  }
  
  
  if(checky) { # use `y`
    .rcpp_set_attr(out, "names", names(y))
    return(invisible(NULL))
  }
  
  
  return(invisible(NULL))
  
}

