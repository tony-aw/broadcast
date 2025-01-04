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
.is_sandwich_orthogonal <- function(x.dim, y.dim) {
  n <- length(x.dim) # = length(y.dim)
  if(n < 3L) {
    return(FALSE)
  }
  else if(n == 3L) {
    if(x.dim[2L] != y.dim[2L]) {
      return(TRUE)
    }
  }
  else {
    ind <- seq(2L, length(x.dim) - 1L)
    if(.C_dims_all_orthogonal(x.dim[ind], y.dim[ind])) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

#' @keywords internal
#' @noRd
.determine_dimmode <- function(x, y, out.dim, abortcall) {
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  # use vector mode:
  if(length(x) == 1L || length(y) == 1L) { # x and/or y are/is scalar(s)
    return(1L)
  }
  else if(is.null(x.dim) || is.null(y.dim)) { # x and/or y are/is vector(s)
    return(1L)
  }
  else if(all(x.dim == y.dim)) { # x & y have same dimensions, thus same ordering as output
    return(1L)
  }
  
  # use orthogonal vectors mode:
  if(length(x.dim) <= 2L && length(y.dim) <= 2 && .C_dims_all_orthogonal(x.dim, y.dim)) {
    return(2L)
  }
  
  # use big-small mode:
  if(all(x.dim == out.dim) || all(y.dim == out.dim)) {
    return(3L)
  }
  
  # use general mode:
  return(4L)
  
}


#' @keywords internal
#' @noRd
.determine_out.dim <- function(x.dim, y.dim, abortcall) {
  if(is.null(x.dim) && is.null(y.dim)) {
    return(NULL)
  }
  else if(!is.null(x.dim) && !is.null(y.dim)) {
    out.dim <- .C_pmax(x.dim, y.dim)
    maxint <- 2^31 - 1
    if(any(out.dim >= maxint)) {
      stop(simpleError("broadcasting will exceed maximum dimension size", call = abortcall))
    }
    out.len <- prod(out.dim)
    max_longvector <- 2^52 - 1
    if(out.len >= max_longvector) {
      stop(simpleError("broadcasting will exceed maximum vector size", call = abortcall))
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
.make_sandwich_params <- function(x.dim, y.dim) {
  n <- length(x.dim)
  if(x.dim[1L] > 1L && x.dim[2L] == 1L) {
    xstarts <- TRUE
  }
  else {
    xstarts <- FALSE
  }
  by_first_last <- c(0L, 0L)
  if(x.dim[1L] == y.dim[1L]) {
    by_first_last[1L] <- 1L
  }
  if(x.dim[n] == y.dim[n]) {
    by_first_last[2L] <- 1L
  }
  
  dcp_x <- .make_dcp(x.dim)
  dcp_y <- .make_dcp(y.dim)
  
  out <- list(
    xstarts = xstarts,
    dcp_x = dcp_x,
    dxp_y = dcp_y,
    by_first_last = by_first_last
  )
  return(out)
}


#' @keywords internal
#' @noRd
.make_by <- function(target.dim, out.dim) {
  # this approach is faster than ifelse()
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
.ndims <- function(x) {
  return(length(dim(x)))
}


#' @keywords internal
#' @noRd
.prep_arrays <- function(x, y) {
  
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
  
  return(list(x, y))
  
}


#' @keywords internal
#' @noRd
.simplify_arrays <- function(x, y) {

  x.dim <- dim(x)
  y.dim <- dim(y)
  
  # drop common 1L dimensions:
  if(length(x.dim) > 1L && length(y.dim) > 1L) {
    ind.drop <- which((x.dim == 1L) & (y.dim == 1L))
    if(length(ind.drop) > 0L) {
      x.dim <- x.dim[-ind.drop]
      y.dim <- y.dim[-ind.drop]
    }
    if(length(x.dim) == 0L) x.dim <- NULL
    if(length(y.dim) == 0L) y.dim <- NULL
    dim(x) <- x.dim
    dim(y) <- y.dim
  }
  x.dim <- dim(x)
  y.dim <- dim(y)
  
  
  # merge mergeable dimensions:
  
  # 2 ADJECENT dimensions of x and y can be merged if they are BOTH NOT auto-orthogonal.
  # i.e. if x.dim[1:2] = c(1, 1) and y.dim[1:2] = c(2, 3),
  # x.dim[1:2] can be merged to become 1 and y.dim[1:2] to become 6 (= prod(c(2, 3))).
  # But if x.dim[1:3] = c(1, 9, 1) and y.dim = c(8, 1, 8),
  # x.dim[1:3] is auto-orthogonal, and so is y.dim[1:3], and thus they CANNOT be merged.
  # Merging prevents unnecessary broadcasting,
  # and I have found it to be a simple but effective optimization method for broadcasting.
  
  if(length(x.dim) > 2L && length(y.dim) > 2L) {
    
    maxint <- 2L^31L - 1L
    
    for(i in 1:length(x.dim)) { # start loop
      
      irle <- .C_findfirst_mergable_dims(x.dim == 1L, y.dim == 1L)
      if(irle[1] != 0L && irle[2] != 0L) { # start if statements
        
        # only merge if the products are less than the integer limit
        x.prod <- prod(x.dim[irle])
        y.prod <- prod(y.dim[irle])
        checkprod <- x.prod < maxint && y.prod < maxint
        if(checkprod) {
          
          if(irle[1] == 1) { # merge at start
            x.dim <- c(prod(x.dim[irle]), x.dim[-irle])
            y.dim <- c(prod(y.dim[irle]), y.dim[-irle])
          }
          else if(irle[2] == length(x.dim)) { # merge at end
            x.dim <- c(x.dim[-irle], prod(x.dim[irle]))
            y.dim <- c(y.dim[-irle], prod(y.dim[irle]))
          }
          else { # merge in between
            first <- 1L:(irle[1] - 1L)
            last <- (irle[2] + 1):length(x.dim)
            between <- irle[1]:irle[2]
            x.dim <- c(x.dim[first], prod(x.dim[between]), x.dim[last])
            y.dim <- c(y.dim[first], prod(y.dim[between]), y.dim[last])
          }
          
        }
      } # end if statements
      
    } # end loop
    
    dim(x) <- x.dim
    dim(y) <- y.dim
      
  }
  
  
  return(list(x, y))
}


.determine_relmode <- function(x, y) {
  if(is.double(x) || is.double(y)) {
    return(1L)
  }
}
