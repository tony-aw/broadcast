

txt1 <- "

// [[Rcpp::export(rcpp_factor_count)]]
int rcpp_factor_count(
  SEXP grp, int j
) {
  const int *pgrp = INTEGER_RO(grp);
  int n = Rf_length(grp);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if(pgrp[i] == j) {
      count++;
    }
  }
  return count;
}

// [[Rcpp::export(rcpp_factor_which)]]
SEXP rcpp_factor_which(
  SEXP grp, int j, int size
) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int *pout = INTEGER(out);
  const int *pgrp = INTEGER_RO(grp);
  int n = Rf_length(grp);
  int count = 0;
  
  for(int i  = 0; i < n; ++i) {
    if(pgrp[i] == j) {
      pout[count] = i + 1;
      count++;
    }
  }
  
  UNPROTECT(1);
  return out;
  
}


// [[Rcpp::export(rcpp_acast)]]
void rcpp_acast(
  SEXP out, SEXP y, const SEXP starts, const SEXP lens, const SEXP subs,
  const SEXP dcp_out, const SEXP dcp_y, SEXP grp, int grp_n, int margin, int newdim
) {
  
  int grp_count = 0;
  SEXP grp_which;
  int *plens = INTEGER(lens);
  int *pstarts = INTEGER(starts);
  margin = margin - 1;
  newdim = newdim - 1;
  
  MACRO_OP_ACAST;
  
}




"


header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  macro_acast
)


header_for_package <- "

#include <Rcpp/Lightest>
#include \"broadcast.h\"

using namespace Rcpp;


"


txt <- stringi::stri_c(
  header_for_sourcing,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_acast.cpp")

