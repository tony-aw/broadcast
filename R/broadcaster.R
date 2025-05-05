#' Check or Set if an Array is a Broadcaster
#'
#' @description
#' `broadcaster()` checks if an array is of class "broadcaster". \cr
#' `broadcaster()<-` sets or un-sets the class attribute "broadcaster" on an array. \cr
#' \cr
#' The `broadcaster` class is a simple (or "lazy") class,
#' and exists purely to overload the math, Boolean, and relational infix operators
#' to support broadcasting. \cr
#' This makes mathematical expressions with multiple variables,
#' where precedence may be important,
#' far more convenient. \cr
#' Like in the following calculation: \cr
#' `x / (y + z)` \cr
#' \cr
#' 
#' The `broadcaster` class comes with its own method dispatch for the base operators. \cr
#' If at least one of the 2 arguments of the base operators is of class `broadcaster`,
#' and no other class (like `bit64`) interferes,
#' broadcasting will occur in the same manner as used in the various `bc.*` - functions. \cr
#' \cr
#'
#' @param x object to check or set.
#' @param value set to `TRUE` to make an array a broadcaster,
#' or `FALSE` to remove the broadcaster class attribute from an array.
#' 
#' 
#' @returns
#' For `broadcaster()`: \cr
#' `TRUE` if an array or vector is a broadcaster, or `FALSE` if it is not. \cr
#' \cr
#' For `broadcaster()<-`: \cr
#' Returns nothing,
#' but sets (if right hand side is `TRUE`)
#' or removes (if right hand side is `TRUE`)
#' the "broadcaster" class attribute. \cr \cr
#'
#'
#' @example inst/examples/broadcaster.R
#' 


#' @rdname broadcaster
#' @export
broadcaster <- function(x) {
  if(!.couldb.broadcaster(x)) return(FALSE)
  return(inherits(x, "broadcaster"))
}


#' @rdname broadcaster
#' @export
`broadcaster<-` <- function(x, value) {
  if(!isTRUE(value) && !isFALSE(value)) {
    stop("right hand side value must be `TRUE` or `FALSE`")
  }
  
  # main function:
  if(!value && !broadcaster(x)) {
    x
  }
  else if(value && broadcaster(x)) {
    x
  }
  else if(!value && broadcaster(x)) {
    class(x) <- setdiff(class(x), "broadcaster")
    x
  }
  else if(value && !broadcaster(x)) {
    if(!.couldb.broadcaster(x)) {
      stop("cannot make this object broadcaster")
    }
    oc <- .internal_sane_class(x)
    class(x) <- c("broadcaster", oc)
    x
  }
  else {
    stop("malformed object given")
  }
}



#' @keywords internal
#' @noRd
.as.broadcaster <- function(x) {
  if(broadcaster(x)) {
    return(x)
  }
  else {
    broadcaster(x) <- TRUE
    return(x)
  }
}



#' @keywords internal
#' @noRd
.internal_sane_class <- function(x) {
  if(is.null(oldClass(x)) && is.null(dim(x))) {
    return(NULL)
  }
  oc <- setdiff(class(x), "broadcaster")
  if(length(oc) == 0L) {
    return(NULL)
  }
  else {
    return(oc)
  }
}


#' @keywords internal
#' @noRd
.couldb.broadcaster <- function(x) {
  return(.is_array_like(x))
}

