#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_make_dcp ( SEXP xdim ) {


int n = Rf_length(xdim);
if(n == 0) {
  return(Rf_ScalarInteger(1));
}

SEXP out = PROTECT(Rf_allocVector(REALSXP, n + 1));
double *pout = REAL(out);
const int *pxdim = INTEGER_RO(xdim);
double temp_prod = pxdim[0];
pout[0] = 1;
pout[1] = pxdim[0];

for(int i = 2; i < (n+1); ++i) {
  temp_prod = temp_prod * pxdim[i-1];
  pout[i] = temp_prod;
}

UNPROTECT(1);
return out;
  
}