
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_bindhelper_sum_along(
    SEXP lst_dims, SEXP along0
) {
  int along = INTEGER(along0)[0];
  int n = Rf_length(lst_dims);
  SEXP tempdim;
  R_xlen_t out = 0;
  for(int i = 0; i < n; ++i) {
    tempdim = VECTOR_ELT(lst_dims, i);
    out += INTEGER(tempdim)[along];
  }
  
  return Rf_ScalarReal(out);
}
