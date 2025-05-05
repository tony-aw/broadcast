
#' @keywords internal
#' @noRd
.overload_get_env4form <- function(f, alt.env) {
  env <- environment(f)
  if(is.null(env)) {
    env <- alt.env
  }
  return(env)
}
