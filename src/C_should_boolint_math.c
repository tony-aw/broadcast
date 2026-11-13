#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_should_boolint_math ( SEXP x, SEXP y, SEXP op0 ) {


  if(Rf_xlength(op0) == 0) {
    return Rf_ScalarLogical(0);
  }
  const int op = INTEGER_RO(op0)[0];
  if(op == 0) {
    return Rf_ScalarLogical(0);
  }
  
  bool checkx = TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP;
  bool checky = TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP;
  bool checkop = op > 0;
  if(checkx && checky && checkop) {
    return Rf_ScalarLogical(1);
  }
  
  return Rf_ScalarLogical(0);


}