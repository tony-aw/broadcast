#include <Rcpp/Lightest>
#include "broadcast.h"
using namespace Rcpp;


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_round2int53)]]
SEXP rcpp_round2int53 ( SEXP x ) {


  const R_xlen_t n = Rf_xlength(x);
  const double int53max = DEF_INT53MAX;
  const double int53min = -1 * int53max;
  
  const double *px = REAL_RO(x);
  
  NumericVector res(no_init(n));
  double *pres = REAL(res);
  
  for(R_xlen_t i = 0; i < n; ++i) {
    pres[i] = inline_int53_round(px[i], int53min, int53max);
  }
  
  return res;

}