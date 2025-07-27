

#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_attr)]]
void rcpp_set_attr(
    RObject x, String name, RObject value
  ) {
    x.attr(name) = value;
  }