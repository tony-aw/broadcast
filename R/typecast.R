
#' @rdname typecast
#' @export
as_bool <- function(x, ...) {
  
  out <- as.logical(x, ...)
  
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}


#' @rdname typecast
#' @export
as_int <- function(x, ...) {
  
  out <- as.integer(x, ...)
  
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}


#' @rdname typecast
#' @export
as_dbl <- function(x, ...) {
  
  out <- as.double(x, ...)
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}

#' @rdname typecast
#' @export
as_num <- as_dbl


#' @rdname typecast
#' @export
as_chr <- function(x, ...) {
  
  out <- as.character(x, ...)
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}


#' @rdname typecast
#' @export
as_str <- as_chr



#' @rdname typecast
#' @export
as_cplx <- function(x, ...) {
  
  out <- as.complex(x, ...)
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}


#' @rdname typecast
#' @export
as_raw <- function(x, ...) {
  
  out <- as.raw(x, ...)
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  return(out)
}


#' @rdname typecast
#' @export
as_list <- function(x, ...) {
  
  out <- as.list(x, ...)
  if(.rcpp_address(out) == .rcpp_address(x)) {
    .typecast_attr(out) <- x
  }
  else {
    .typecast_setattr(out, x)
  }
  
  return(out)
}

#' @keywords internal
#' @noRd
`.typecast_attr<-` <- function(x, value) {
  if(length(value) == length(x)) {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    comment(x) <- comment(value)
    if(broadcaster(value) && !broadcaster(x) && .couldb.broadcaster(x)) {
      class(x) <- c(oldClass(x), "broadcaster")
    }
    if(is.atomic(x) && is.atomic(value) && inherits(value, "mutatomic") && !inherits(x, "mutatomic")) {
      class(x) <- c("mutatomic", oldClass(x))
      attr(x, "serial")<- attr(value, "serial", exact = TRUE)
    }
  }
  x
}


#' @keywords internal
#' @noRd
.typecast_setattr <- function(x, value) {
  if(length(value) == length(x)) {
    .rcpp_set_attr(x, "dim", dim(value))
    .set_dimnames(x, dimnames(value))
    .rcpp_set_attr(x, "names", names(value))
    .rcpp_set_attr(x, "comment", comment(value))
    if(broadcaster(value) && !broadcaster(x) && .couldb.broadcaster(x)) {
      .rcpp_set_attr(x, "class", c(oldClass(x), "broadcaster"))
    }
    if(is.atomic(x) && is.atomic(value) && inherits(value, "mutatomic") && !inherits(x, "mutatomic")) {
      .rcpp_set_attr(x, "class",  c("mutatomic", oldClass(x)))
      .rcpp_set_attr(x, "serial", attr(value, "serial", exact = TRUE))
    }
  }
}
