
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_bindhelper_get_alongdims(
    SEXP lst, SEXP along0
) {
  
  const int along = INTEGER_RO(along0)[0];
  
  int n = Rf_length(lst);
  SEXP temp;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *pout = INTEGER(out);
  
  for(int i = 0; i < n; ++i) {
    temp = VECTOR_ELT(lst, i);
    pout[i] = INTEGER(temp)[along];
  }
  UNPROTECT(1);
  
  return out;
}
