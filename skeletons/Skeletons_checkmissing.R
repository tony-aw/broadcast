# set-up ====

library(stringi)


header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  macro_checkmissing,
  "\n"
)


header_for_package <- "

#include <Rcpp/Lightest>
#include \"broadcast.h\"

using namespace Rcpp;


"

macro_checkmissing <- readr::read_file("macro_checkmissing.txt")

################################################################################
# code ====
#

code <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_is_raw)]]
SEXP rcpp_checkmissing_is_raw(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, n));
  Rbyte *pout = RAW(out);
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_IS, pout[i]);
  
  UNPROTECT(1);
  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_is_logical)]]
SEXP rcpp_checkmissing_is_logical(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *pout = LOGICAL(out);
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_IS, pout[i]);
  
  UNPROTECT(1);
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_count)]]
R_xlen_t rcpp_checkmissing_count(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  
  R_xlen_t count = 0;
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_FW, count++);
  return count;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_first)]]
R_xlen_t rcpp_checkmissing_first(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_FW, return i + 1);
  return 0;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_last)]]
R_xlen_t rcpp_checkmissing_last(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_BW, return i + 1);
  return 0;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_which32)]]
SEXP rcpp_checkmissing_which32(
  SEXP y, bool invert, int size
) {
  R_xlen_t n = Rf_xlength(y);
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int *pout = INTEGER(out);
  int counter = 0;
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_FW, pout[counter] = i + 1; counter++);
  
  UNPROTECT(1);
  return out;
  
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_which64)]]
SEXP rcpp_checkmissing_which64(
  SEXP y, bool invert, int size
) {
  R_xlen_t n = Rf_xlength(y);
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double *pout = REAL(out);
  R_xlen_t counter = 0;
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_FW, pout[counter] = i + 1; counter++);
  
  UNPROTECT(1);
  return out;
  
}



"


################################################################################
# write code ====
#


txt <- stringi::stri_c(
  header_for_sourcing,
  code,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")

txt <- stringi::stri_c(
  header_for_package,
  code,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_checkmissing.cpp")


