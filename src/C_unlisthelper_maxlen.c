#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_unlisthelper_maxlen ( SEXP x ) {


  int n = Rf_xlength(x);
  R_xlen_t maxlen = pow(2.0, 31.0) - 1.0;
  double out = 0;
  for(R_xlen_t i = 0; i < n; ++i) {
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