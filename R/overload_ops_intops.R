

#' @export
`%%.broadcaster` <- function(e1, e2) {
  .overloadS3_intop(e1, e2, "%%", sys.call())
}

#' @export
`%/%.broadcaster` <- function(e1, e2) {
  .overloadS3_intop(e1, e2, "%/%", sys.call())
}

#' @keywords internal
#' @noRd
.overloadS3_intop <- function(e1, e2, op, abortcall) {
  return(.bc_i(e1, e2, op, abortcall))
}
