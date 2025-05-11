

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


