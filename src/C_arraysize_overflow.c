#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_arraysize_overflow(SEXP xdim, SEXP xlen0) {
  
  double xlen;
  if(TYPEOF(xlen0) == INTSXP) {
    xlen = (double) INTEGER(xlen0)[0];
  }
  else {
    xlen = REAL(xlen0)[0];
  }
  
  const double maxlong = pow(2, 52) - 1;
  if(xlen > maxlong) {
    return Rf_ScalarLogical(1);
  }
  
  int n = Rf_xlength(xdim);
  const int *pxdim = INTEGER_RO(xdim);
  const int maxint = pow(2, 31) - 1;
  for(int i  = 0; i < n; ++i) {
    if(pxdim[i] > maxint || pxdim[i] < 0) {
      return Rf_ScalarLogical(1);
    }
  }
  return Rf_ScalarLogical(0);
}