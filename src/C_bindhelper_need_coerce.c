
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_bindhelper_need_coerce(
    SEXP lst, SEXP mytemplate
) {
  
  int n = Rf_length(lst);
  SEXP temp;
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, n));
  Rbyte *pout = RAW(out);
  
  for(int i = 0; i < n; ++i) {
    temp = VECTOR_ELT(lst, i);
    pout[i] = TYPEOF(temp) != TYPEOF(mytemplate);
  }
  UNPROTECT(1);
  
  return out;
}
