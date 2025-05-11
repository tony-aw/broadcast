

#' @export
`==.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "==", sys.call())
  
  if(is.character(e1) || is.character(e2)) {
    if(!is.character(e1)) e1 <- as_str(e1)
    if(!is.character(e2)) e2 <- as_str(e2)
    return(.bc_str_rel(e1, e2, 1L, sys.call()))
  }
  else if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    return(.bc_cplx_rel(e1, e2, 1L, sys.call()))
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 1L, 0, sys.call()))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, 1L, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @export
`!=.broadcaster` <- function(e1, e2) {
  .binary_stop_general(e1, e2, "!=", sys.call())
  
  if(is.character(e1) || is.character(e2)) {
    if(!is.character(e1)) e1 <- as_str(e1)
    if(!is.character(e2)) e2 <- as_str(e2)
    return(.bc_str_rel(e1, e2, 2L, sys.call()))
  }
  else if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    return(.bc_cplx_rel(e1, e2, 2L, sys.call()))
  }
  else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 2L, 0, sys.call()))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, 2L, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @export
`<.broadcaster` <- function(e1, e2) {
  
  .binary_stop_general(e1, e2, "<", sys.call())
  
  if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 3L, 0, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @export
`>.broadcaster` <- function(e1, e2) {
  
  .binary_stop_general(e1, e2, ">", sys.call())
  
  if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 4L, 0, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}



#' @export
`<=.broadcaster` <- function(e1, e2) {
  
  .binary_stop_general(e1, e2, "<=", sys.call())
  
  if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 5L, 0, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @export
`>=.broadcaster` <- function(e1, e2) {
  
  .binary_stop_general(e1, e2, ">=", sys.call())
  
  if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
    return(.bc_dec_rel(e1, e2, 6L, 0, sys.call()))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @keywords internal
bc_overloaded_relops <- list(
  `==` =  `==.broadcaster`,
  `!=` =  `!=.broadcaster`,
  `<` = `<.broadcaster`,
  `>` = `>.broadcaster`,
  `<=` = `<=.broadcaster`,
  `>=` = `>=.broadcaster`
)
