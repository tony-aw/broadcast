

#' @export
`==.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 1L, sys.call())
}


#' @export
`!=.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 2L, sys.call())
}


#' @export
`<.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 3L, sys.call())
}


#' @export
`>.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 4L, sys.call())
}



#' @export
`<=.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 5L, sys.call())
}


#' @export
`>=.broadcaster` <- function(e1, e2) {
  .overload_relop_gs(e1, e2, 6L, sys.call())
}


#' @keywords internal
#' @noRd
.overload_relop_equneq <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  if(is.character(e1) || is.character(e2)) {
    if(!is.character(e1)) e1 <- as_str(e1)
    if(!is.character(e2)) e2 <- as_str(e2)
    return(.bc_str_rel(e1, e2, op, abortcall))
  }
  else if(is.complex(e1) || is.complex(e2)) {
    if(!is.complex(e1)) e1 <- as_cplx(e1)
    if(!is.complex(e2)) e2 <- as_cplx(e2)
    return(.bc_cplx_rel(e1, e2, op, abortcall))
  }
  else if(is.numeric(e1) || is.numeric(e2)) {
    if(is.raw(e1)) e1 <- as_int(e1)
    if(is.raw(e2)) e2 <- as_int(e2)
    return(.bc_dec_rel(e1, e2, op, 0, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    return(.bc_b(e1, e2, op + 4L, abortcall))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop("unsupported combination of types given")
  }
}


#' @keywords internal
#' @noRd
.overload_relop_gs <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  if(is.numeric(e1) || is.numeric(e2)) {
    if(is.raw(e1)) e1 <- as_int(e1)
    if(is.raw(e2)) e2 <- as_int(e2)
    return(.bc_dec_rel(e1, e2, op, 0, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    return(.bc_b(e1, e2, op + 4L))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop(simpleError("unsupported combination of types given", call = abortcall))
  }
}
