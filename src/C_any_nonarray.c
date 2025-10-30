#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_any_nonarray ( SEXP x ) {


  if(TYPEOF(x) != VECSXP) {
    error("`x` must be a list");
  }
  
  const int n = Rf_length(x);
  SEXP tempout;
  SEXP tempdim;
  
  
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    if(Rf_length(tempdim) == 0) {
      return(Rf_ScalarLogical(1));
    }
    if(TYPEOF(tempdim) != INTSXP) {
      return(Rf_ScalarLogical(1));
    }
    const int *ptemp = INTEGER_RO(tempdim);
    const int m = Rf_length(tempdim);
    for(int j = 0; j < m; ++j) {
      if(ptemp[j] < 0) {
        return(Rf_ScalarLogical(1));
      }
    }
  }
  return Rf_ScalarLogical(0);


}