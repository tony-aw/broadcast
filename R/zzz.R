.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    "?broadcast::broadcast",
    "` to open the introduction help page of 'broadcast'."
  )
  packageStartupMessage(txt)
}
