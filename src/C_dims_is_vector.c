#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_dims_is_vector(SEXP x) {
  
  if(x == R_NilValue) {
    return Rf_ScalarLogical(1);
  }
  
  const int n = Rf_xlength(x);
  const int *px = INTEGER_RO(x);
  int count_big = 0;
  
  for(int i = 0; i < n; ++i) {
    count_big += px[i] > 1;
  }
  
  
  return Rf_ScalarLogical(count_big <= 1);
}