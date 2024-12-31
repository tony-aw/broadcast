#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_findfirst_range_cons_dupl)]]
IntegerVector rcpp_findfirst_range_cons_dupl(
  LogicalVector x
) {
  int n = Rf_length(x);
  IntegerVector startend = {0, 0};
  int idx;
  int found = 0;
  
  for(int i = 0; i < (n - 1); ++i) {
    idx = i + 1;
    if(found == 0 && x[i] && x[i + 1]) {
      startend[0] = idx;
      found = 1;
    }
    if(found > 0 && x[i] && x[i + 1]) {
      startend[1] = idx + 1;
    }
    if(found > 0 && (!x[i] || !x[i + 1])) {
      return(startend);
    }
  }
  
  return startend;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_is_chesslike)]]
bool rcpp_is_chesslike(
  IntegerVector x, IntegerVector y
) {
  int n = Rf_length(x);
  for(int i = 1; i < n; ++i) {
    if(x[i] == x[i-1] || y[i] == y[i-1]) {
      return false;
    }
  }
  return true;
}
