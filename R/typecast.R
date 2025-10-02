
#' @rdname typecast
#' @export
as_bool <- function(x, ...) {
  
  out <- as.logical(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @rdname typecast
#' @export
as_int <- function(x, ...) {
  
  out <- as.integer(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @rdname typecast
#' @export
as_dbl <- function(x, ...) {
  
  out <- as.double(x, ...)
  .typecast(out) <- x
  return(out)
}

#' @rdname typecast
#' @export
as_num <- as_dbl


#' @rdname typecast
#' @export
as_chr <- function(x, ...) {
  
  out <- as.character(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @rdname typecast
#' @export
as_str <- as_chr



#' @rdname typecast
#' @export
as_cplx <- function(x, ...) {
  
  out <- as.complex(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @rdname typecast
#' @export
as_raw <- function(x, ...) {
  out <- as.raw(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @rdname typecast
#' @export
as_list <- function(x, ...) {
  out <- as.list(x, ...)
  .typecast(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
`.typecast<-` <- function(x, value) {
  if(length(value) == length(x)) {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    broadcaster(x) <- broadcaster(value)
  }
  x
}
