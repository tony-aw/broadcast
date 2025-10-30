#


#' @keywords internal
#' @noRd
.binary_stop_general <- function(x, y, op, abortcall) {
  if(!.is_array_like(x) || !.is_array_like(y)) {
    stop(simpleError("input must be arrays or simple vecors", call = abortcall))
  }
  if(ndim(x) > 16L || ndim(y) > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  if(!is.character(op) || length(op) != 1L) {
    stop(simpleError("`op` must be single string", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.binary_prep <- function(x, y, abortcall) {
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  x.len <- length(x)
  y.len <- length(y)
  if(!is.null(x.dim) || !is.null(y.dim)) {
    if(is.null(x.dim)) x.dim <- x.len
    if(is.null(y.dim)) y.dim <- y.len
  }
  
  
  
  ##############################################################################
  # normalize dimensions ====
  if(!is.null(x.dim) && !is.null(y.dim)) {
    xndim <- length(x.dim)
    yndim <- length(y.dim)
    if(xndim > yndim) {
      y.dim <- c(y.dim, rep(1L, xndim - yndim))
    }
    if(yndim > xndim) {
      x.dim <- c(x.dim, rep(1L, yndim - xndim))
    }
  }
  
  
  
  ##############################################################################
  # Check & determine dimensions to return====
  .binary_stop_conf_dim(x.dim, y.dim, x.len, y.len, abortcall)
  out.dimorig <- .binary_determine_out.dim(x.dim, y.dim, abortcall)
  out.len <- .binary_determine_out.len(x.dim, y.dim, x.len, y.len, out.dimorig)
  
  
  
  ##############################################################################
  # Simplify array dimensions, to reduce broadcast load ====
  # Note that all of this is done AFTER Normalization (see few sections above)
  
  # drop dimensions for scalars:
  if(x.len == 1L) {
    x.dim <- NULL
  }
  if(y.len == 1L) {
    y.dim <- NULL
  }
  
  # drop dimensions for vectors under certain conditions:
  if(length(x.dim) <= 1L && length(y.dim) <= 1L) {
    # if both are 1d arrays or vectors, drop dimensions
    # if only one is a 1d array, DON'T drop dimensions,
    # since it may be orthogonal broadcasting
    # (i.e. colvector * rowvector = 1d * 2d)
    x.dim <- NULL
    y.dim <- NULL
  }
  # end dropping dimensions for vectors and scalars
  
  
  # drop common 1L dimensions:
  if(length(x.dim) > 1L && length(y.dim) > 1L) {
    drop.count <- .C_dropdims_count(x.dim, y.dim)
    if(drop.count > 0L) {
      drop.ind <- .C_dropdims_which(x.dim, y.dim, drop.count)
      x.dim <- x.dim[-drop.ind]
      y.dim <- y.dim[-drop.ind]
    }
    if(length(x.dim) == 0L) x.dim <- NULL
    if(length(y.dim) == 0L) y.dim <- NULL
  } # end dropping dimensions
  
  
  # merge mergeable dimensions:
  
  # 2 ADJACENT dimensions of x and y can be merged if they are BOTH NOT auto-orthogonal.
  # i.e. if x.dim[1:2] = c(1, 1) and y.dim[1:2] = c(2, 3),
  # x.dim[1:2] can be merged to become 1 and y.dim[1:2] to become 6 (= prod(c(2, 3))).
  # But if x.dim[1:3] = c(1, 9, 1) and y.dim = c(8, 1, 8),
  # x.dim[1:3] is auto-orthogonal, and so is y.dim[1:3], and thus they CANNOT be merged.
  # Merging prevents unnecessary broadcasting,
  # which in turn makes the actual broadcasting more efficient.
  
  if(length(x.dim) > 2L && length(y.dim) > 2L) {
    # NOT relevant if ndim <= 2L !!!
    mergeable <- .rcpp_is_mergeable_with_prev(x.dim == 1L, y.dim == 1L)
    if(any(mergeable)) {
      mergedims <- .rcpp_mergedims(x.dim, y.dim, mergeable)
      x.dim <- mergedims[[1L]]
      y.dim <- mergedims[[2L]]
    }
    
  }
  ndim <- length(x.dim)
  out.dimsimp <- .binary_determine_out.dim(x.dim, y.dim, abortcall)
  
  
  
  ##############################################################################
  # Determine type of dimensional relationship for broadcasting ====
  
  dimmode <- .C_determine_dimmode(x.dim, y.dim, x.len, y.len)
  
  
  
  ##############################################################################
  # Chunkify Dimensions ====
  
  if(dimmode == 3L && ndim == 2L) { # sandwichify dimensions so that they fit the B2V MACRO
    if(x.dim[1L] > 1L && y.dim[1L] > 1L) { # vector dims = (n, 1)
      x.dim <- c(1L, x.dim)
      y.dim <- c(1L, y.dim)
      out.dimsimp <- .C_pmax(x.dim, y.dim)
      ndim <- length(x.dim)
    }
    else if(x.dim[2L] > 1L && x.dim[2L] > 1L) { # vector dims = (1, n)
      x.dim <- c(x.dim, 1L)
      y.dim <- c(y.dim, 1L)
      out.dimsimp <- .C_pmax(x.dim, y.dim)
      ndim <- length(x.dim)
    }
  }
  if(dimmode == 4L && ndim < 16L) { # make dimensions of length 16 so that they fit the general MACRO
    x.dim <- .chunkify_dims(x.dim)
    y.dim <-  .chunkify_dims(y.dim)
    out.dimsimp <- .C_pmax(x.dim, y.dim)
    ndim <- 16L
  }
  
  
  ##############################################################################
  # Return list ====
  out <- list(
    x.dim = x.dim,
    y.dim = y.dim,
    out.dimorig = out.dimorig,
    out.dimsimp = out.dimsimp,
    out.len = out.len,
    dimmode = dimmode
  )
  
  return(out)
  
}




#' @keywords internal
#' @noRd
.binary_stop_conf_dim <- function(x.dim, y.dim, x.len, y.len, abortcall) {
  
  out <- .C_check_conf_dim(x.dim, y.dim, x.len, y.len)
  if(!out) {
    stop(simpleError("`x` and `y` are not conformable", call = abortcall))
  }
}



#' @keywords internal
#' @noRd
.binary_determine_out.dim <- function(x.dim, y.dim, abortcall) {
  if(is.null(x.dim) && is.null(y.dim)) {
    return(NULL)
  }
  else if(!is.null(x.dim) && !is.null(y.dim)) {
    out.dim <- .C_pmax(x.dim, y.dim)
    out.len <- prod(out.dim)
    if(.C_arraysize_overflow(out.dim, out.len)){
      stop(simpleError("broadcasting will exceed maximum size", call = abortcall))
    }
    return(out.dim)
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
.binary_determine_out.len <- function(x.dim, y.dim, x.len, y.len, out.dim) {
  if(is.null(x.dim) || is.null(y.dim)) {
    return(max(x.len, y.len))
  }
  else {
    return(prod(out.dim))
  }
  
}




#' @keywords internal
#' @noRd
.binary_set_attr <- function(out, x, y) {
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  
  if(is.atomic(out) && (inherits(x, "mutatomic") || inherits(y, "mutatomic"))) {
    .rcpp_set_ma(out, c("mutatomic", oldClass(out)))
  }
  
  .binames_set(x, y, out)
  
}

