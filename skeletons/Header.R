# set-up ====

library(stringi)
# dMacro_skeletons <- qs::qread("dMacro_skeletons")
# macros <- stri_c(dMacro_skeletons, collapse = "\n")



macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")
macro_op <- readr::read_file("macro_op.txt")
macro_acast <- readr::read_file("macro_acast.txt")

header <- stri_c("

#ifndef BROADCAST_H
#define BROADCAST_H

",
macro_action,
"\n",
macro_typeswitch_numeric,
"\n",
macro_op,
"\n",
macro_dim,
"\n",
macro_acast,

"

#endif
"
)

cat(header)
Rcpp::sourceCpp(code = header)
readr::write_file(header, "header.txt")
setwd("..")
readr::write_file(header, "src/broadcast.h")

