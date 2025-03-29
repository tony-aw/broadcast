
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
    temp.dimnames <- .bind_getnames(arg.dimnames[[i]], arg.names[i], marginlen) # NOTE: arg.names[i] works, even if arg.names is NULL...
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
  n <- ndim(obj)
  
  if(along == 0) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[seq(2, n + 1L)] == dim(obj)) # replaced dim(out)[2:n] with dim(out)[seq(2, n + 1L)]
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



#' @keywords internal
#' @noRd
.bind_mat_dimnames <- function(out, input, along, name_deparse, comnames_from) {
  
  out.dimnames <- dimnames(out)
  
  # make along variables:
  along <- as.integer(along)
  if(along == 1L) not_along <- 2L
  if(along == 2L) not_along <- 1L
  
  
  # clear dimnames if there should be no names, and return:
  if(name_deparse == 0L && is.null(comnames_from)) {
    return(NULL)
  }
  
  # stop the rest of the function if `out` is not a matrix:
  if(ndim(out) < 2 || !is.matrix(out)) {
    return(dimnames(out))
  }
  
  # remove alongnames if unneeded:
  if(!name_deparse && !is.null(out.dimnames)) {
    out.dimnames[along] <- list(NULL)
  }
  
  # early return if no comnames needed:
  if(is.null(comnames_from)) {
    return(out.dimnames)
  }
  
  # recreate comnames:
  comarg <- input[[comnames_from]]
  check1 <- c(
    is.array(comarg),
    !is.null(dimnames(comarg)),
    length(dimnames(comarg)) >= not_along,
    !is.null(dimnames(comarg)[[not_along]])
  )
  if(all(check1)) {
    if(!is.null(out.dimnames)) {
      out.dimnames[not_along] <- list(NULL)
    }
    if(is.null(out.dimnames) && !is.null(comnames_from)) {
      out.dimnames <- rep(list(NULL), ndim(out))
    }
    out.dimnames[not_along] <- dimnames(comarg)[not_along]
  }
  check2 <- !is.array(comarg) && !is.null(names(comarg))
  if(check2) {
    if(!is.null(out.dimnames)) {
      out.dimnames[not_along] <- list(NULL)
    }
    if(is.null(out.dimnames) && !is.null(comnames_from)) {
      out.dimnames <- rep(list(NULL), ndim(out))
    }
    out.dimnames[[not_along]] <- names(comarg)
  }
  if(!all(check1) && !check2) {
    out.dimnames
  }
  
  return(out.dimnames)
  
}
