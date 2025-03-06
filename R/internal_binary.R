#


#' @keywords internal
#' @noRd
.binay_stop_general <- function(x, y, op, abortcall) {
  if(!.is_array_like(x) || !.is_array_like(y)) {
    stop(simpleError("input must be arrays or simple vecors", call = abortcall))
  }
  if(ndim(x) > 16L || ndim(y) > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  if(length(x) == 0L || length(y) == 0L) {
    stop(simpleError("zero-length objects not supported", call = abortcall))
  }
  if(!is.character(op) || length(op) != 1L) {
    stop(simpleError("`op` must be single string", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.binay_prep <- function(x, y, abortcall) {
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  x.len <- length(x)
  y.len <- length(y)
  if(is.null(x.dim)) x.dim <- x.len
  if(is.null(y.dim)) y.dim <- y.len
  
  # normalize dimensions:
  prep <- .normalize_dims(x.dim, y.dim)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  
  
  # Check & determine dimensions to return:
  .stop_conf_dim(x.dim, y.dim, x.len, y.len, abortcall)
  out.dimorig <- .binay_determine_out.dim(x.dim, y.dim, abortcall)
  out.len <- .binay_determine_out.len(x.dim, y.dim, x.len, y.len, out.dimorig)
  
  
  # Simplify arrays, to reduce broadcast load:
  simp <- .simplify_dims(x.dim, y.dim, x.len, y.len)
  x.dim <- simp[[1L]]
  y.dim <- simp[[2L]]
  out.dimsimp <- .binay_determine_out.dim(x.dim, y.dim, abortcall)
  
  # Determine type of dimensional relationship for broadcasting:
  dimmode <- .binay_determine_dimmode(x.dim, y.dim, x.len, y.len, out.dimsimp, abortcall)
  
  out <- list(
    x.dim = x.dim,
    y.dim = y.dim,
    x.len = x.len,
    y.len = y.len,
    out.dimorig = out.dimorig,
    out.dimsimp = out.dimsimp,
    out.len = out.len,
    dimmode = dimmode
  )
  
  return(out)
  
}



#' @keywords internal
#' @noRd
.stop_conf_dim <- function(x.dim, y.dim, x.len, y.len, abortcall) {
  
  if(is.null(x.dim) || is.null(y.dim)) {
    if(x.len != y.len) {
      if(x.len != 1 && y.len != 1) {
        stop(simpleError("`x` and `y` are not conformable", call = abortcall))
      }
    }
  }
  else {
    out <- .C_check_conf_dim(x.dim, y.dim)
    if(!out) {
      stop(simpleError("`x` and `y` are not conformable", call = abortcall))
    }
  }
}

#' @keywords internal
#' @noRd
.binay_determine_dimmode <- function(x.dim, y.dim, x.len, y.len, out.dim, abortcall) {
  
  # use vector mode:
  if(x.len == 1L || y.len == 1L) { # x and/or y are/is scalar(s)
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
.binay_determine_out.dim <- function(x.dim, y.dim, abortcall) {
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
.binay_determine_out.len <- function(x.dim, y.dim, x.len, y.len, out.dim) {
  if(is.null(x.dim) || is.null(y.dim)) {
    return(max(x.len, y.len))
  }
  else {
    return(prod(out.dim))
  }
  
}



#' @keywords internal
#' @noRd
.normalize_dims <- function(x.dim, y.dim) {
  
  # normalize dimensions:
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
  
  return(list(x.dim, y.dim))
  
}



#' @keywords internal
#' @noRd
.simplify_dims <- function(x.dim, y.dim, x.len, y.len) {
  
  # drop dimensions for vectors and scalars:
  if(x.len == 1L) {
    x.dim <- NULL
  }
  if(y.len == 1L) {
    y.dim <- NULL
  }
  if(length(x.dim) <= 1L && length(y.dim) <= 1L) {
    # if both are 1d arrays or vectors, drop dimensions
    # if only one is a 1d array, DON'T drop dimensions,
    # since it may be orthogonal broadcasting
    # (i.e. colvector * rowvector = 1d * 2d)
    # also, 1d %op% matrix is not the same as vector %op% matrix when broadcasted
    x.dim <- NULL
    y.dim <- NULL
  }
  # end dropping dimensions for vectors and scalars
  
  
  # drop common 1L dimensions:
  if(length(x.dim) > 1L && length(y.dim) > 1L) {
    ind.drop <- which((x.dim == 1L) & (y.dim == 1L))
    if(length(ind.drop) > 0L) {
      x.dim <- x.dim[-ind.drop]
      y.dim <- y.dim[-ind.drop]
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
  # and I have found it to be a simple but effective optimization method for broadcasting.
  
  if(length(x.dim) > 2L && length(y.dim) > 2L) {
    
    maxint <- 2L^31L - 1L
    
    for(i in 1:length(x.dim)) { # start loop
      
      irle <- .C_findfirst_mergable_dims(x.dim == 1L, y.dim == 1L)
      if(irle[1] != 0L && irle[2] != 0L) { # start if statements
        
        # only merge if the products are less than the integer limit
        rng <- irle[1]:irle[2]
        x.prod <- prod(x.dim[rng])
        y.prod <- prod(y.dim[rng])
        checkprod <- x.prod < maxint && y.prod < maxint
        if(checkprod) {
          
          if(irle[1] == 1) { # merge at start
            x.dim <- c(x.prod, x.dim[-rng])
            y.dim <- c(y.prod, y.dim[-rng])
          }
          else if(irle[2] == length(x.dim)) { # merge at end
            x.dim <- c(x.dim[-rng], x.prod)
            y.dim <- c(y.dim[-rng], y.prod)
          }
          else { # merge in between
            first <- 1L:(irle[1] - 1L)
            last <- (irle[2] + 1):length(x.dim)
            x.dim <- c(x.dim[first], x.prod, x.dim[last])
            y.dim <- c(y.dim[first], y.prod, y.dim[last])
          }
          
        }
      } # end if statements
      
    } # end loop
    
  } # end merging
  
  
  # chunkify dimensions of arrays:
  # chunkification allows reduction of the amount of required compiled code,
  # thus reducing compilaion & installation time of the package
  if(length(x.dim) > 2L && length(y.dim) > 2L) {
    xndim <- length(x.dim)
    yndim <- length(y.dim)
    
    if(!.is.even(xndim)) {
      x.dim <- c(x.dim, 1L)
    }
    if(!.is.even(yndim)) {
      y.dim <- c(y.dim, 1L)
    }
  } # end chunkification
  
  
  return(list(x.dim, y.dim))
}


