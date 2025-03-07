
#' @keywords internal
#' @noRd
.bind_stop_name_along <- function(
    name_along, comnames_from, abortcall
) {
  
  if(!is.logical(name_along) || length(name_along) != 1) {
    stop(simpleError("`name_along` must be a Boolean", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bind_stop_name_deparse <- function(
    name_deparse, abortcall
) {
  
  if(!is.logical(name_deparse) || length(name_deparse) != 1) {
    stop(simpleError("`name_deparse` must be a Boolean", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bind_stop_comnames_from <- function(
    comnames_from, abortcall
) {
  
  if(!is.null(comnames_from)) {
    if(!is.numeric(comnames_from) || length(comnames_from) != 1) {
      stop(simpleError("`comnames_from` must be an integer scalar or `NULL`", call = abortcall))
    }
  }
}

#' @keywords internal
#' @noRd
.bind_name_along_reasonable <- function(input, arg.dimnames) {
  return(.C_any_nonNULL(arg.dimnames) || !is.null(names(input)))
}


#' @keywords internal
#' @noRd
.bind_get_alongnames <- function(
    out, along, input, arg.dimnames, arg.marginlen
) {
  # this function is only run when along != 0 and along != (N+1)
  name_along <- vector(mode = "character", length = dim(out)[along])
  arg.names <- names(input)
  start.pos <- 0L
  for(i in seq_along(input)) {
    marginlen <- arg.marginlen[i]
    indx <- seq_len(marginlen) + start.pos
    temp.dimnames <- .bind_getnames(arg.dimnames[[i]], arg.names[i], marginlen)
    .rcpp_set_vind_32_atomic(name_along, indx - 1L, temp.dimnames)
    start.pos <- start.pos + marginlen
  }
  out.dimnames <- rep(list(NULL), length(dim(out)))
  out.dimnames[[along]] <- name_along
  return(out.dimnames)
}


#' @keywords internal
#' @noRd
.bind_inplace_comnames <- function(out, sel, input, along) {
  
  # general prep:
  out.dimnames <- dimnames(out)
  if(is.null(out.dimnames)) {
    out.dimnames <- rep(list(NULL), length(dim(out)))
  }
  obj <- input[[sel]]
  obj.dimnames <- dimnames(obj)
  n <- length(dim(obj))
  
  if(along == 0) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[2:n] == dim(obj))
      if(length(ind) > 0L) {
        out.dimnames[ind + 1L] <- obj.dimnames[ind]
        dimnames(out) <- out.dimnames # this is a shallow copy
      }
    }
    return(out)
  }
  
  input.dims <- .rcpp_bindhelper_vdims(input)
  max_ndims <- max(lengths(input.dims))
  if(along > max_ndims) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[1:n] == dim(obj))
      if(length(ind) > 0L) {
        out.dimnames[ind] <- obj.dimnames[ind]
        dimnames(out) <- out.dimnames
      }
    }
    return(out)
  }
  
  
  if(!is.null(obj.dimnames)) {
    ind <- which(dim(out)[1:n] == dim(obj))
    ind <- ind[ind != along]
    if(length(ind) > 0L) {
      out.dimnames[ind] <- obj.dimnames[ind]
      dimnames(out) <- out.dimnames
    }
  }
  return(out)
  
}


#' @keywords internal
#' @noRd
.bind_getnames <- function(main.names, arg.name, size) {
  if(!is.null(main.names)) {
    temp.names <- main.names
  }
  else if(!is.null(arg.name)) {
    if(size > 1L) {
      temp.names <- paste0(arg.name, ".", seq_len(size))
    }
    else {
      temp.names <- arg.name
    }
  }
  else {
    temp.names <- ""
  }
  return(temp.names)
}
