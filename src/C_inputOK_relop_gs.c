#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_inputOK_relop_gs ( SEXP x, SEXP y ) {
  int xOK = TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP || TYPEOF(x) == RAWSXP || TYPEOF(x) == LGLSXP;
  int yOK = TYPEOF(y) == INTSXP || TYPEOF(y) == REALSXP || TYPEOF(y) == RAWSXP || TYPEOF(y) == LGLSXP;
  if(xOK && yOK) {
    return Rf_ScalarLogical(1);
  }
  return Rf_ScalarLogical(0);
}