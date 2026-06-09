# set-up ====

library(stringi)
# dMacro_skeletons <- qs::qread("dMacro_skeletons")
# macros <- stri_c(dMacro_skeletons, collapse = "\n")


inlines <- readr::read_file("inlines.txt")
macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")
macro_op <- readr::read_file("macro_op.txt")
macro_acast <- readr::read_file("macro_acast.txt")
macro_checkmissing <- readr::read_file("macro_checkmissing.txt")
macro_ternary <- readr::read_file("macro_ternary.txt")

header <- stri_c("

#ifndef BROADCAST_H
#define BROADCAST_H

#include <Rcpp.h>

",
inlines,
"\n",
macro_action,
"\n",
macro_typeswitch_numeric,
"\n",
macro_op,
"\n",
macro_dim,
"\n",
macro_acast,
"\n",
macro_checkmissing,

"

#endif
"
)

cat(header)
readr::write_file(header, "header.txt")
Rcpp::sourceCpp(code = header)
setwd("..")
readr::write_file(header, "src/broadcast.h")

