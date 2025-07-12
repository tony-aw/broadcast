#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_any_nonNULL ( SEXP x ) {

int n = Rf_xlength(x);
const SEXP *px = (SEXP *) DATAPTR_RO(x);
for(int i = 0; i < n; ++i) {
  if(px[i] != R_NilValue) {
    return Rf_ScalarLogical(1);
  }
}
return Rf_ScalarLogical(0);
}