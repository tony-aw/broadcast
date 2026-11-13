

#' @export
`&.broadcaster` <- function(e1, e2) {
  return(.overloadS3_andor(e1, e2, "&", sys.call()))
}

#' @export
`|.broadcaster` <- function(e1, e2) {
  return(.overloadS3_andor(e1, e2, "|", sys.call()))
}

#' @keywords internal
#' @noRd
.overloadS3_andor <- function(e1, e2, op, abortcall) {
  .binary_stop_general(e1, e2, "?", abortcall)
  
  
  if(is.complex(e1) || is.double(e1)) {
    e1 <- as_bool(e1)
  }
  if(is.complex(e2) || is.double(e2)) {
    e2 <- as_bool(e2)
  }
  
  if(.is_int32(e1) && .is_int32(e2)) {
    return(.bc_b(e1, e2, op, sys.call()))
  }
  else if(is.raw(e1) && is.raw(e2)) {
    return(.bc_bit(e1, e2, op, sys.call()))
  }
  else {
    stop(simpleError(
      "operations are possible only for numeric, logical or complex types",
      call = abortcall
    ))
  }

}
