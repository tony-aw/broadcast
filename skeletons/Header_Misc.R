# set-up ====

library(stringi)


macro_acast <- readr::read_file("txt/macro_acast.txt")
macro_checkmissing <- readr::read_file("txt/macro_checkmissing.txt")

header_body <- stri_c(
  macro_acast,
  "\n",
  macro_checkmissing
)

header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  ",
  header_body,
  "\n"
)


header_for_package <- stri_c("

#ifndef MISC_H
#define MISC_H

#include <Rcpp/Lightest>

",
header_body,

"

#endif
"
)


Rcpp::sourceCpp(code = header_for_sourcing)

readr::write_file(header_for_package, "header_misc.txt")

setwd("..")
readr::write_file(header_for_package, "src/misc.h")

