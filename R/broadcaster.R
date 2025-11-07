#' Check or Set if an Array is a Broadcaster
#'
#' @description
#' `broadcaster()` checks if an array or vector has the "broadcaster" attribute. \cr
#' `bcr()` is a short-hand alias for `broadcaster()`. \cr
#' \cr
#' `broadcaster()<-` (or `bcr()<-`) sets or un-sets the class attribute "broadcaster" on an array or vector. \cr
#' \cr
#' The `broadcaster` class attribute exists purely to overload the
#' arithmetic, Boolean, bit-wise, and relational infix operators,
#' to support broadcasting. \cr
#' This makes mathematical expressions with multiple variables,
#' where precedence may be important,
#' far more convenient. \cr
#' Like in the following calculation: \cr
#' `x / (y + z)` \cr
#' \cr
#' See \link{broadcast_operators} for more information. \cr
#'
#' @param x object to check or set. \cr
#' Only S3 vectors and arrays are supported, and only up to 16 dimensions.
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
#' or removes (if right hand side is `FALSE`)
#' the "broadcaster" class attribute. \cr \cr
#'
#' @seealso \link{broadcast_operators} \cr
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
    class(x) <- setdiff(oldClass(x), "broadcaster")
    x
  }
  else if(value && !broadcaster(x)) {
    if(!.couldb.broadcaster(x)) {
      stop("cannot make this object broadcaster")
    }
    class(x) <- c(oldClass(x), "broadcaster")
    x
  }
  else {
    stop("malformed object given")
  }
}

#' @rdname broadcaster
#' @export
bcr <- broadcaster

#' @rdname broadcaster
#' @export
`bcr<-` <- `broadcaster<-`



#' @keywords internal
#' @noRd
.as.broadcaster <- function(x) {
  if(!.couldb.broadcaster(x)) {
    stop("cannot make this object broadcaster")
  }
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
.couldb.broadcaster <- function(x) {
  return(.is_array_like(x) && .is_supported_type(x) && ndim(x) <= 16L)
}

setOldClass("broadcaster")

