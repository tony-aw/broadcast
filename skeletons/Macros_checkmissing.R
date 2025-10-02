# set-up ====

library(stringi)

header <- "

#include <Rcpp/Lightest>
  
  using namespace Rcpp;

"

Rcpp::sourceCpp(code = macro_checkmissing_loop)

