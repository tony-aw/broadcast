# set-up ====

library(stringi)
# dMacro_skeletons <- qs::qread("dMacro_skeletons")
# macros <- stri_c(dMacro_skeletons, collapse = "\n")



macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")

header <- stri_c("

#ifndef MACROS_EVERYWHERE_H
#define MACROS_EVERYWHERE_H

",
macro_action,
"\n",
macro_dim,
"\n",
macro_typeswitch_numeric,
"\n",

"

#endif
"
)

cat(header)
Rcpp::sourceCpp(code = header)
readr::write_file(header, "src/Macros_Everywhere.h")

