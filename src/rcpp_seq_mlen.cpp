#include <Rcpp/Lightest>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_recycle_seq_mlen)]]
List rcpp_recycle_seq_mlen(IntegerVector x, IntegerVector y) {
  int n = x.length();
  List out(n);
  for(int i = 0; i < n; ++i) {
    int size = y[i];
    IntegerVector temp(size);
    int counter = 1;
    for(int j = 0; j < size; ++j) {
      temp[j] = counter;
      counter++;
      if(counter > x[i]) {
        counter = 1;
      }
    }
    out[i] = temp;
  }
  return out;
}