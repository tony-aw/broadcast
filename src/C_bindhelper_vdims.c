
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_bindhelper_vdims(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  SEXP tempdim;
  
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    SET_VECTOR_ELT(out, i, tempdim);
  }
  
  UNPROTECT(1);
  return out;
}
