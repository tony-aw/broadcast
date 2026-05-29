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
  
  
  ##############################################################################
  # normalize dimensions ====
  
  if(!is.null(x.dim) || !is.null(y.dim)) {
    intmax <- 2^31 - 1L
    if(is.null(x.dim)) {
      if(x.len > intmax) {
        stop(simpleError("broadcasting will exceed maximum size"))
      }
      x.dim <- x.len
    }
    if(is.null(y.dim)) {
      if(y.len > intmax) {
        stop(simpleError("broadcasting will exceed maximum size"))
      }
      y.dim <- y.len
    }
    
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
  # Check & determine dimensions to return ====
  .binary_stop_conf_dim(x.dim, y.dim, x.len, y.len, abortcall)
  out.dimorig <- .C_make_outdim(x.dim, y.dim)
  out.len <- .binary_determine_out.len(out.dimorig, x.len, y.len)
  
  
  
  ##############################################################################
  # Simplify array dimensions, to reduce broadcast load ====
  # Note that all of this is done AFTER Normalization (see few sections above)
  
  # drop dimensions under certain conditions:
  # if at least one of x or y is a scalar, we don't need the dimensions any more.
  # if BOTH are vectors, we don't need dimensions any more.
  
  if(x.len == 1L || y.len == 1L || (length(x.dim) <= 1L && length(y.dim) <= 1L)) {
    x.dim <- NULL
    y.dim <- NULL
  }
  # end dropping dimensions
  
  
  # drop common 1L dimensions:
  if(length(x.dim) > 1L && length(y.dim) > 1L) {
    ind <- x.dim != 1L | y.dim != 1L ## equivalent to !(x.dim == 1L && y.dim == 1L)
    if(any(ind)) {
      x.dim <- x.dim[ind]
      y.dim <- y.dim[ind]
    }
    else {
      x.dim <- NULL
      y.dim <- NULL
    }
  } # end drop common 1L dimensions
  
  
  # merge mergeable dimensions:
  if(length(x.dim) > 2L && length(y.dim) > 2L) {
    mergedims <- .rcpp_mergedims(x.dim, y.dim)
    x.dim <- mergedims[[1L]]
    y.dim <- mergedims[[2L]]
    
  }
  out.dimsimp <- .C_make_outdim(x.dim, y.dim)
  
  
  
  ##############################################################################
  # Determine type of dimensional relationship for broadcasting ====
  
  dimmode <- .C_determine_dimmode(x.dim, y.dim, x.len, y.len)
  
  
  
  ##############################################################################
  # Chunkify Dimensions ====
  
  ndim <- max(length(x.dim), length(y.dim)) # only used for chunkification
  if(dimmode == 3L && ndim == 2L) { # sandwichify dimensions so that they fit the B2V MACRO
    if(x.dim[1L] > 1L && y.dim[1L] > 1L) { # vector dims = (n, 1)
      x.dim <- c(1L, x.dim)
      y.dim <- c(1L, y.dim)
      out.dimsimp <- .C_pmax(x.dim, y.dim)
      ndim <- length(x.dim)
    }
    else if(x.dim[2L] > 1L && y.dim[2L] > 1L) { # vector dims = (1, n)
      x.dim <- c(x.dim, 1L)
      y.dim <- c(y.dim, 1L)
      out.dimsimp <- .C_pmax(x.dim, y.dim)
      ndim <- length(x.dim)
    }
  }
  if(dimmode >= 4L && ndim < 16L) { # make dimensions fit the general MACRO
    x.dim <- .chunkify_dims(x.dim)
    y.dim <-  .chunkify_dims(y.dim)
    out.dimsimp <- .C_pmax(x.dim, y.dim)
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
.binary_determine_out.len <- function(out.dim, x.len, y.len) {
  if(is.null(out.dim)) {
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
  
  .rcpp_binames_set(x, y, out)
  .binary_set_comm(x, y, out)
  
}

#' @keywords internal
#' @noRd
.binary_set_attr_logical <- function(out, x, y) {
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  
  .rcpp_binames_set(x, y, out)
  
}



#' @keywords internal
#' @noRd
.binary_return_zerolen <- function(x, y, is_logical_op = FALSE, returntype = NULL) {
  
  # determine output type & and make output of type:
  out.type <- returntype
  if(is.null(returntype)) {
    out.type <- .C_bindhelper_max_type(list(x, y))
    out.type <- .types()[out.type]
  }
  out <- vector(out.type, 0L)
  
  # length INDEPENDENT attributes:
  if(broadcaster(x) || broadcaster(y)) {
    broadcaster(out) <- TRUE
  }
  
  if(!is_logical_op) {
    .binary_set_comm(x, y, out)
  }
  
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.binary_set_comm <- function(x, y, out) {
  x.comm <- comment(x)
  y.comm <- comment(y)
  x.hascom <- !is.null(x.comm)
  y.hascom <- !is.null(y.comm)
  if(x.hascom != y.hascom) {
    if(x.hascom) {
      .rcpp_set_attr(out, "comment", x.comm)
    }
    else {
      .rcpp_set_attr(out, "comment", y.comm)
    }
  }
}
