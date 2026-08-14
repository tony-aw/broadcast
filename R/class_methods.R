

#' @export
chooseOpsMethod.broadcaster <- function(x, y, mx, my, cl, reverse) FALSE


#' @export
`[.broadcaster` <- function(x, ..., drop = FALSE) {
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  y <- NextMethod("[")
  class(y) <- oldClass(x)
  y
}


#' @export
`[[.broadcaster` <- function(x, ...) {
  
  if(!broadcaster(x)) {
    stop("malformed broadcaster")
  }
  y <- NextMethod("[[")
  class(y) <- oldClass(x)
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


