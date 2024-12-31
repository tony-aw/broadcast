#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_pmax ( SEXP x, SEXP y ) {

  
  const int *restrict px = INTEGER(x);
  const int *restrict py = INTEGER(y);
  
  int n = Rf_length(x);
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *pout;
  pout = INTEGER(out);
  
  for(int i = 0; i < n; ++i) {
    if(px[i] > py[i]) {
      pout[i] = px[i];
    }
    else {
      pout[i] = py[i];
    }
  }
  
  UNPROTECT(1);
  return(out);

}