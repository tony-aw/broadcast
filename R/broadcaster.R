#' Check or Set if an Array is a Broadcaster
#'
#' @description
#' `broadcaster()` checks if an array or vector has the "broadcaster" attribute. \cr
#' `bcr()` is a short-hand alias for `broadcaster()`. \cr
#' \cr
#' `broadcaster()<-` (or `bcr()<-`) sets or un-sets the class attribute "broadcaster" on an array or vector. \cr
#' \cr
#' `mbroadcasters()` sets or un-sets multiple objects in an environment as broadcaster. \cr
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
#' @param nms a character vector of variable names.
#' @param env the environment where to look for the variable names specified in `nms`. \cr
#' If `NULL`, the environment from which the function was called is used. \cr
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
#' the "broadcaster" class attribute. \cr
#' \cr
#' For `mbroadcasters()`: \cr
#' Returns nothing,
#' but sets (if `value = TRUE`)
#' or removes (`value = FALSE`)
#' the "broadcaster" class attribute. \cr
#' If `value = TRUE`, 
#' objects that cannot become a broadcaster or are already a broadcaster
#' will be ignored. \cr
#' If `value = FALSE`, 
#' objects that are not broadcasters (according to `broadcaster()`)
#' will be ignored. \cr
#' \cr
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
mbroadcasters <- function(nms, value, env = NULL) {
  
  # checks:
  if(!is.character(nms) || !length(nms)) {
    stop("`nms` must be a character vector")
  }
  if(!isTRUE(value) && !isFALSE(value)) {
    stop("`value` must be `TRUE` or `FALSE`")
  }
  if(!is.null(env) && !is.environment(env)) {
    stop("`env` must be an environment or `NULL`")
  }
  
  # make env if necessary:
  if(is.null(env)) {
    env <- parent.frame(n = 1L)
  }
  
  # NOTE: using string evaluation instead of direct environment access for 2 reasons:
  # 1) Environment access may lead to memory leak
  # 2) Environment access results (according to my tests) to unnecessary copying,
  #     whereas directly evaluating something like `class(x) <- "broadcaster"` does not create unnecessary copies.
  # Why is 'R' sometimes so bad at memory handling?
  
  # FUNCTION:
  if(isTRUE(value)) {
    for(i in nms) {
      txt <- sprintf(
        "if(!broadcaster(%s) && broadcast:::.couldb.broadcaster(%s)) { 
          class(%s) <- c(oldClass(%s), 'broadcaster')
        }",
        i, i, i, i
      )
      expr <- parse(text = txt)
      eval(expr, env)
    }
  }
  if(isFALSE(value)) {
    for(i in nms) {
      txt <- sprintf("if(broadcaster(%s)) class(%s) <- setdiff(oldClass(%s), 'broadcaster')", i, i, i)
      expr <- parse(text = txt)
      eval(expr, env)
    }
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

