#include <Rcpp/Lightest>

using namespace Rcpp;



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_clone)]]
SEXP rcpp_clone(RObject x) {
  SEXP y = clone(x);
  return y;
}
