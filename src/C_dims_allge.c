#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_dims_allge(SEXP x, SEXP y) {
  int n = Rf_xlength(x);
  const int *px = INTEGER_RO(x);
  const int *py = INTEGER_RO(y);
  for(int i  = 0; i < n; ++i) {
    if(px[i] < py[i]) {
      return Rf_ScalarLogical(0);
    }
  }
  return Rf_ScalarLogical(1);
}