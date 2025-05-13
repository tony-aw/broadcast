

#' @export
`+.broadcaster` <- function(e1, e2) {
  if(missing(e2)) {
    return(e1) 
  }
  .binary_stop_general(e1, e2, "+", sys.call())
  a <- .binary_attr(e1, e2)
  op <- 1L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_bit(e1, e2, op+2L, sys.call()))
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
  
}


#' @export
`-.broadcaster` <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- e1
    e1 <- 0L
  }
  .binary_stop_general(e1, e2, "-", sys.call())
  a <- .binary_attr(e1, e2)
  
  op <- 2L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_bit(e1, e2, op+2L, sys.call()))
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}



#' @export
`*.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "*", sys.call())
  a <- .binary_attr(e1, e2)
  
  op <- 3L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_bit(e1, e2, op+2L, sys.call()))
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}



#' @export
`/.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "/", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, 4L, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, 4L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}


#' @export
`^.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "^", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`^` operator not (yet) supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, 5L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}


#' @export
`%%.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "%%", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`%%` operator not supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_int_math(e1, e2, 5L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}

#' @export
`%/%.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "%/%", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`%/%` operator not supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_int_math(e1, e2, 6L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}


#' @keywords internal
bc_overloaded_mathops <- list(
  `+` =  `+.broadcaster`,
  `-` =  `-.broadcaster`,
  `*` =  `*.broadcaster`,
  `/` =  `/.broadcaster`,
  `^` =  `^.broadcaster`,
  `%%` =  `%%.broadcaster`,
  `%/%` =  `%/%.broadcaster`
)
