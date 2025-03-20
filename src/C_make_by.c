#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_make_by ( SEXP target_dim ) {


int n = Rf_length(target_dim);

SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
int *pout = INTEGER(out);
const int *ptarget = INTEGER_RO(target_dim);

for(int i = 0; i < n; ++i) {
  if(ptarget[i] > 1) {
    pout[i] = 1;
  }
  else {
    pout[i] = 0;
  }
}

UNPROTECT(1);
return out;



}