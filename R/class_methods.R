
#' @export
as.logical.broadcaster <- function(x, ...) {
  out <- as.logical(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}


#' @export
as.integer.broadcaster <- function(x, ...) {
  out <- as.integer(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}


#' @export
as.double.broadcaster <- function(x, ...) {
  out <- as.double(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}



#' @export
as.complex.broadcaster <- function(x, ...) {
  out <- as.complex(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}



#' @export
as.character.broadcaster <- function(x, ...) {
  out <- as.character(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}


#' @export
as.raw.broadcaster <- function(x, ...) {
  out <- as.raw(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}

#' @export
as.list.broadcaster <- function(x, ...) {
  out <- as.list(unclass(x))
  .broadcaster_typecast(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
`.broadcaster_typecast<-` <- function(x, value) {
  if(!broadcaster(value)) {
    x
  }
  else {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    class(x) <- c("broadcaster", .internal_sane_class(value))
    x
  }
  
}


#' @export
c.broadcaster <- function(..., use.names = TRUE) {
  y <- unlist(list(...), recursive = FALSE, use.names = use.names)
  oc <- .internal_sane_class(y)
  class(y) <- c("broadcaster", oc)
  return(y)
}


#' @export
`[.broadcaster` <- function(x, ..., drop = FALSE) {
  
  drop <- FALSE
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  y <- NextMethod("[")
  
  if(!inherits(y, "broadcaster")) {
    class(y) <- c("broadcaster", .internal_sane_class(y))
  }
  
  y
}


#' @export
`[[.broadcaster` <- function(x, ...) {
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  y <- NextMethod("[[")
  
  if(!inherits(y, "broadcaster")) {
    class(y) <- c("broadcaster", .internal_sane_class(y))
  }
  y
}



#' @export
format.broadcaster <- function(x, ...) {
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  
  class(x) <- setdiff(class(x), "broadcaster")
  format(x, ...)
}


#' @export
print.broadcaster <- function(x, ...) {
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  
  class(x) <- setdiff(class(x), "broadcaster")
  print(x, ...)
  cat("broadcaster \n")
}


