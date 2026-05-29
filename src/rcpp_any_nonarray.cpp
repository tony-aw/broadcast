#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_any_nonarray)]]
bool rcpp_any_nonarray ( SEXP x ) {


  if(TYPEOF(x) != VECSXP) {
    stop("`x` must be a list");
  }
  
  const int n = Rf_length(x);
  RObject tempout;
  RObject tempdim;
  
  
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = tempout.attr("dim");
    if(Rf_length(tempdim) == 0) {
      return true;
    }
    if(TYPEOF(tempdim) != INTSXP) {
      return true;
    }
    const int *ptemp = INTEGER_RO(tempdim);
    const int m = Rf_length(tempdim);
    for(int j = 0; j < m; ++j) {
      if(ptemp[j] < 0) {
        return true;
      }
    }
  }
  return false;


}
