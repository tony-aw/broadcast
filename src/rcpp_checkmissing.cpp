

#include <Rcpp/Lightest>
#include "misc.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_is_raw)]]
SEXP rcpp_checkmissing_is_raw(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  
  RawVector out(no_init(n));
  Rbyte *pout = RAW(out);
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_IS, pout[i]);

  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_checkmissing_is_logical)]]
SEXP rcpp_checkmissing_is_logical(
  SEXP y, bool invert
) {
  R_xlen_t n = Rf_xlength(y);
  
  LogicalVector out(no_init(n));
  int *pout = LOGICAL(out);
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_IS, pout[i]);
  
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
// [[Rcpp::export(.rcpp_checkmissing_which64)]]
SEXP rcpp_checkmissing_which64(
  SEXP y, bool invert, R_xlen_t size
) {
  R_xlen_t n = Rf_xlength(y);
  
  NumericVector out(no_init(size));
  double *pout = REAL(out);
  R_xlen_t counter = 0;
  
  MACRO_CHECKMISSING_TYPESWITCH(MACRO_CHECKMISSING_FW, pout[counter] = i + 1; counter++);
  
  return out;
  
}



