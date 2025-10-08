#include <Rcpp/Lightest>
using namespace Rcpp;


inline SEXP rcpp_rep_new_int(
  int n, int val
) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *pout = INTEGER(out);
  for(int i = 0; i < n; ++i) {
    pout[i] = val;
  }
  
  UNPROTECT(1);
  return(out);
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_normalize_dims)]]
SEXP rcpp_normalize_dims_regular(
  SEXP dims_old, int start, int max_ndim
) {
  int n = Rf_length(dims_old);
  
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  
  for(int i = 0; i < n; ++i) {
    SEXP temp_new = rcpp_rep_new_int(max_ndim, 1);
    SEXP temp_old = VECTOR_ELT(dims_old, i);
    
    int m = Rf_length(temp_old);
    
    int *pnew = INTEGER(temp_new);
    const int *pold = INTEGER_RO(temp_old);
    
    for(int j = 0; j < m; ++j) {
      pnew[j + start] = pold[j];
    }
    
    SET_VECTOR_ELT(out, i, temp_new);
    
  }
  
  UNPROTECT(1);
  return out;
  
  
}