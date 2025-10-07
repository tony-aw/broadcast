

#' @export
`+.broadcaster` <- function(e1, e2) {
  if(missing(e2)) {
    return(e1) 
  }
  .binary_stop_general(e1, e2, "+", sys.call())
  
  op <- 1L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
  
}


#' @export
`-.broadcaster` <- function(e1, e2) {
  if (missing(e2)) {
    y <- NextMethod("-")
    return(y)
  }
  .binary_stop_general(e1, e2, "-", sys.call())
  
  
  op <- 2L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}



#' @export
`*.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "*", sys.call())
  
  
  op <- 3L
  if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    out <- .bc_cplx_math(e1, e2, op, sys.call())
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, op, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}



#' @export
`/.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "/", sys.call())
  
  
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
  
 
  return(out)
}


#' @export
`^.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "^", sys.call())
  
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`^` operator not (yet) supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_dec_math(e1, e2, 5L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}


#' @export
`%%.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "%%", sys.call())
  
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`%%` operator not supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_int_fact(e1, e2, 2L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}

#' @export
`%/%.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "%/%", sys.call())
  
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`%/%` operator not supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_int_fact(e1, e2, 3L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}
