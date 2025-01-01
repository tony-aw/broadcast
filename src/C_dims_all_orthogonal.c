#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_dims_all_orthogonal(
  SEXP xdim, SEXP ydim
) {
  
  int n = Rf_length(xdim);
  
  const int *px = INTEGER_RO(xdim);
  const int *py = INTEGER_RO(ydim);
  
  for(int i = 1; i < n; ++i) {
    if(px[i-1] == py[i-1]) {
      return Rf_ScalarLogical(0);
    }
    if(px[i] == py[i]) {
      return Rf_ScalarLogical(0);
    }
    if(px[i] == px[i-1] || py[i] == py[i-1]) {
      return Rf_ScalarLogical(0);
    }
  }
  return Rf_ScalarLogical(1);
}
