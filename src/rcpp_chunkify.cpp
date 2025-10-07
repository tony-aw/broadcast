
#include <Rcpp/Lightest>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_chunk_size)]]
int rcpp_chunk_size(int ndim, SEXP chunks) {

  int *pchunks = INTEGER(chunks);
  for(int i = 0; i < Rf_length(chunks); ++i) {
    if(ndim <= pchunks[i]) {
      return pchunks[i];
    }
  }
  
  return 0;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_chunk_set)]]
void rcpp_chunk_set(SEXP dimsold, SEXP dimsnew) {

  const int *pdimsold = INTEGER_RO(dimsold);
  int *pdimsnew = INTEGER(dimsnew);
  int n = Rf_length(dimsold);
  
  for(int i = 0; i < n; ++i) {
    pdimsnew[i] = pdimsold[i];
  }
}
