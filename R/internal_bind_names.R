

#' @keywords internal
#' @noRd
.bind_name_along_reasonable <- function(input, arg.dimnames) {
  return(.C_any_nonNULL(arg.dimnames) || !is.null(names(input)))
}


#' @keywords internal
#' @noRd
.bind_set_alongnames <- function(
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
  dimnames <- rep(list(NULL), length(dim(out)))
  dimnames[[along]] <- name_along
  data.table::setattr(out, "dimnames", dimnames)
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.bind_set_comnames <- function(out, sel, input, along) {
  
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
        data.table::setattr(out, "dimnames", out.dimnames)
      }
    }
    return(invisible(NULL))
  }
  
  input.dims <- .rcpp_bindhelper_vdims(input)
  max_ndims <- max(lengths(input.dims))
  if(along > max_ndims) {
    if(!is.null(obj.dimnames)) {
      ind <- which(dim(out)[1:n] == dim(obj))
      if(length(ind) > 0L) {
        out.dimnames[ind] <- obj.dimnames[ind]
        data.table::setattr(out, "dimnames", out.dimnames)
      }
    }
    return(invisible(NULL))
  }
  
  
  if(!is.null(obj.dimnames)) {
    ind <- which(dim(out)[1:n] == dim(obj))
    ind <- ind[ind != along]
    if(length(ind) > 0L) {
      out.dimnames[ind] <- obj.dimnames[ind]
      data.table::setattr(out, "dimnames", out.dimnames)
    }
  }
  return(invisible(NULL))
  
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
