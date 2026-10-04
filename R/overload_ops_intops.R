

#' @export
`%%.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "%%", sys.call())
  
  
  if(is.complex(e1) || is.complex(e2)) {
    stop("`%%` operator not supported for type `complex`")
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    out <- .bc_int_d(e1, e2, 2L, sys.call())
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
    out <- .bc_int_d(e1, e2, 3L, sys.call())
  }
  else {
    stop("non-numeric argument to binary operator")
  }
  
 
  return(out)
}
