#include <Rcpp/Lightest>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_abind_get_dimnames)]]
List rcpp_abind_get_dimnames(
  List x, int along
) {
  int n = x.length();
  List out(n);
  for(int i = 0; i < n; ++i) {
    RObject temp = x[i];
    if(temp.hasAttribute("dimnames")) {
      List temp2 = temp.attr("dimnames");
      out[i] = temp2[along - 1];
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rcbind_get_sizes)]]
IntegerVector rcpp_rcbind_get_sizes(
    List lst, int imargin
  ) {
    int n = lst.length();
    IntegerVector out(n);
    for(int i = 0; i < n; ++i) {
      RObject temp = lst[i];
      out[i] = Rf_length(temp);
      if(temp.hasAttribute("dim")) {
        IntegerVector dims = temp.attr("dim");
        if(Rf_length(dims) >= (imargin + 1)) {
          out[i] = dims[imargin];
        }
      }
    }
    return out;
  }

