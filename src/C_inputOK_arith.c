#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_inputOK_arith ( SEXP x, SEXP y ) {
  int xOK = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP || TYPEOF(x) == CPLXSXP;
  int yOK = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP || TYPEOF(y) == REALSXP || TYPEOF(y) == CPLXSXP;
  if(xOK && yOK) {
    return Rf_ScalarLogical(1);
  }
  return Rf_ScalarLogical(0);
}