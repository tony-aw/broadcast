
#' @keywords internal
#' @noRd
.bind_stop_name_along <- function(
    name_along, abortcall
) {
  
  if(!isTRUE(name_along) && !isFALSE(name_along)) {
    stop(simpleError("`name_along` must be `TRUE` or `FALSE`", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bind_stop_name_deparse <- function(
    name_deparse, abortcall
) {
  
  if(!isTRUE(name_deparse) && !isFALSE(name_deparse)) {
    stop(simpleError("`name_deparse` must be `TRUE` or `FALSE`", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.bind_stop_comnames_from <- function(
    comnames_from, input, abortcall
) {
  
  if(!is.null(comnames_from)) {
    if(!.is.integer_scalar(comnames_from)) {
      stop(simpleError("`comnames_from` must be an integer scalar or `NULL`", call = abortcall))
    }
    if(comnames_from < 1L || comnames_from > length(input)) {
      stop(simpleError("`comnames_from` out of bounds", call = abortcall))
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
.bind_comnames_reasonable <- function(input_original, along, comnames_from, ndim_max) {
  comnames <- dimnames(input_original[[comnames_from]])
  return(.C_any_nonNULL(comnames)) # detailed checks come later
}


#' @keywords internal
#' @noRd
.bind_prep_dimnames <- function(out) {
  if(is.null(dimnames(out))) {
    out.dimnames <- rep(list(NULL), ndim(out))
    return(out.dimnames)
  }
  else{
    return(dimnames(out))
  }
}

# have to split these functions into "get * names" and "set * names"...

#' @keywords internal
#' @noRd
.bind_get_alongnames <- function(
    out, along, input, arg.dimnames, arg.marginlen
) {
  
  # this function is only run when:
  # along != 0 && along != (N+1) && .bind_name_along_reasonable(...) == TRUE
  
  name_along <- vector(mode = "character", length = dim(out)[along])
  arg.names <- names(input)
  start.pos <- 0L
  for(i in seq_along(input)) {
    marginlen <- arg.marginlen[i]
    indx <- seq_len(marginlen) + start.pos
    temp.dimnames <- .bind_getnames(arg.dimnames[[i]], arg.names[i], marginlen) # NOTE: arg.names[i] works, even if arg.names is NULL...
    .rcpp_set_vind_32(name_along, as.integer(indx - 1L), temp.dimnames)
    start.pos <- start.pos + marginlen
  }
  
  return(name_along)
}


#' @keywords internal
#' @noRd
.bind_which_comnames <- function(out, along, obj, ndim_max) {
  obj.dimnames <- dimnames(obj)
  n <- ndim(obj)
  
  if(along == 0) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[seq(2, n + 1L)] == dim(obj)) # replaced dim(out)[2:n] with dim(out)[seq(2, n + 1L)]
      if(length(ind) > 0L) {
        return(list(out.ind = ind + 1L, obj.ind = ind))
      }
    }
    return(list(NULL, NULL))
  }
  
  if(along > ndim_max) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[1:n] == dim(obj))
      if(length(ind) > 0L) {
        return(list(out.ind = ind, obj.ind = ind))
      }
    }
    return(list(NULL, NULL))
  }
  
  if(!is.null(obj.dimnames)) {
    ind <- which(dim(out)[1:n] == dim(obj))
    ind <- ind[ind != along]
    if(length(ind) > 0L) {
      return(list(out.ind = ind, obj.ind = ind))
    }
    return(list(NULL, NULL))
  }
  
  return(list(NULL, NULL))
}


#' @keywords internal
#' @noRd
.bind_getnames <- function(main.names, arg.name, size) {
  if(!is.null(main.names)) {
    temp.names <- main.names
  }
  else if(!is.null(arg.name)) {
    if(size > 1L) {
      if(is.na(arg.name) || !nzchar(arg.name)) { # arg.name is singular
        temp.names <- rep("", size)
      }
      else {
        temp.names <- paste0(arg.name, ".", seq_len(size))
      }
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

