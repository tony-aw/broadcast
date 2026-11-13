

#' @export
`==.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 1L, sys.call())
}


#' @export
`!=.broadcaster` <- function(e1, e2) {
  .overload_relop_equneq(e1, e2, 2L, sys.call())
}


#' @keywords internal
#' @noRd
.overload_relop_equneq <- function(e1, e2, op, opstr, abortcall) {
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
  else if(is.double(e1) || is.double(e2)) {
    if(is.raw(e1)) e1 <- as_int(e1)
    if(is.raw(e2)) e2 <- as_int(e2)
    return(.bc_dec_rel(e1, e2, op, abortcall))
  }
  else if(is.integer(e1) || is.integer(e2)) {
    if(is.raw(e1)) e1 <- as_int(e1)
    if(is.raw(e2)) e2 <- as_int(e2)
    return(.bc_boolint_rel(e1, e2, op, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    # this is a separate branch so we can safely deal with raw by logical
    # in `.bc_boolint_rel()`
    if(is.raw(e1)) e1 <- as_lgl(e1)
    if(is.raw(e2)) e2 <- as_lgl(e2)
    return(.bc_boolint_rel(e1, e2, op, abortcall))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop("unsupported combination of types given")
  }
}

