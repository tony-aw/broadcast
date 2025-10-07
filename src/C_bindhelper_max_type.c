
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_bindhelper_max_type(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  int out = 1;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    if(TYPEOF(tempout) == VECSXP) {
      out = 8;
    }
    else if(TYPEOF(tempout) == STRSXP && out < 8) {
      out = 7;
    }
    else if(TYPEOF(tempout) == CPLXSXP && out < 7) {
      out = 6;
    }
    else if(TYPEOF(tempout) == REALSXP && out < 6) {
      out = 5;
    }
    else if(TYPEOF(tempout) == INTSXP && out < 5) {
      out = 4;
    }
    else if(TYPEOF(tempout) == LGLSXP && out < 4) {
      out = 3;
    }
    else if(TYPEOF(tempout) == RAWSXP && out < 3) {
      out = 2;
    }
    else if(out < 2){
      out = 1;
    }
  }
  
  return Rf_ScalarInteger(out);
}
