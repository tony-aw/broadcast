
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
.overload_relop_gs <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  if(!.C_inputOK_relop_gs(e1, e2)) {
    stop(simpleError("invalid comparison with given types", call = abortcall))
  }
  
  if(is.numeric(e1) || is.numeric(e2)) {
    if(!is.numeric(e1)) e1 <- as_dbl(e1)
    if(!is.numeric(e2)) e2 <- as_dbl(e2)
    return(.bc_dec_rel(e1, e2, op, abortcall))
  }
  else if(is.logical(e1) || is.logical(e2)) {
    # this is a separate branch so we can safely deal with raw by logical
    if(!is.logical(e1)) e1 <- as_lgl(e1)
    if(!is.logical(e2)) e2 <- as_lgl(e2)
    return(.bc_dec_rel(e1, e2, op, abortcall))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_raw_rel(e1, e2, op, abortcall))
  }
  else {
    stop(simpleError("unsupported combination of types given", call = abortcall))
  }
}

