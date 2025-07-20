
#' @rdname typecast
#' @export
as_bool <- function(x, ...) {
  
  out <- as.logical(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname typecast
#' @export
as_int <- function(x, ...) {
  
  out <- as.integer(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname typecast
#' @export
as_dbl <- function(x, ...) {
  
  out <- as.double(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}

#' @rdname typecast
#' @export
as_num <- as_dbl


#' @rdname typecast
#' @export
as_chr <- function(x, ...) {
  
  out <- as.character(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname typecast
#' @export
as_str <- as_chr



#' @rdname typecast
#' @export
as_cplx <- function(x, ...) {
  
  out <- as.complex(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname typecast
#' @export
as_raw <- function(x, ...) {
  out <- as.raw(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname typecast
#' @export
as_list <- function(x, ...) {
  out <- as.list(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}
