# set-up ====

library(stringi)
# dMacro_skeletons <- qs::qread("dMacro_skeletons")
# macros <- stri_c(dMacro_skeletons, collapse = "\n")


inlines <- readr::read_file("txt/inlines.txt")
macro_dim <- readr::read_file("txt/macro_dim.txt")

header_body <- stri_c(
  inlines,
  "\n",
  macro_dim,
  "\n"
)

header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  ",
  header_body,
  "\n"
)


header_for_package <- stri_c("

#ifndef BROADCAST_H
#define BROADCAST_H

#include <Rcpp/Lightest>

",
header_body,

"

#endif
"
)


Rcpp::sourceCpp(code = header_for_sourcing)

readr::write_file(header_for_package, "header.txt")

setwd("..")
readr::write_file(header_for_package, "src/broadcast.h")

