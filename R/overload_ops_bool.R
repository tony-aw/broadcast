

#' @export
`&.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "&", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.double(e1)) {
    e1 <- as_bool(e1)
  }
  if(is.complex(e2) || is.double(e2)) {
    e2 <- as_bool(e2)
  }
  
  if(.is_logical_like(e1) && .is_logical_like(e2)) {
    out <- .bc_b(e1, e2, 1L, sys.call())
  }
  else if(is.raw(e1) && is.raw(e2)) {
    out <- .bc_raw_bit(e1, e2, 1L, sys.call())
  }
  
  else {
    stop("operations are possible only for numeric, logical or complex types")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}

#' @export
`|.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "|", sys.call())
  a <- .binary_attr(e1, e2)
  
  if(is.complex(e1) || is.double(e1)) {
    e1 <- as_bool(e1)
  }
  if(is.complex(e2) || is.double(e2)) {
    e2 <- as_bool(e2)
  }
  
  if(.is_logical_like(e1) && .is_logical_like(e2)) {
    out <- .bc_b(e1, e2, 2L, sys.call())
  }
  else if(is.raw(e1) && is.raw(e2)) {
    out <- .bc_raw_bit(e1, e2, 2L, sys.call())
  }
  else {
    stop("operations are possible only for numeric, logical or complex types")
  }
  
  attributes(out) <- c(attributes(out), a)
  return(out)
}



#' @keywords internal
bc_overloaded_boolops <- list(
  `&` =  `&.broadcaster`,
  `|` =  `|.broadcaster`
)
