#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_unlisthelper_maxlen ( SEXP x ) {


  int n = Rf_length(x);
  R_xlen_t maxlen = pow(2, 31) - 1;
  double out = 0;
  for(int i = 0; i < n; ++i) {
    SEXP tempout = VECTOR_ELT(x, i);
    R_xlen_t len = Rf_xlength(tempout);
    if(len > maxlen) {
      error("long vectors not supported");
    }
    if(len > out) {
      out = (double)len;
    }
  }
  
  return (ScalarReal(out));

}