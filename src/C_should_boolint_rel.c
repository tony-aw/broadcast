#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_should_boolint_rel( SEXP x, SEXP y, SEXP op0 ) {


  if(Rf_xlength(op0) == 0) {
    return Rf_ScalarLogical(0);
  }
  
  const int check_op = INTEGER(op0)[0] >= 1 && INTEGER(op0)[0] <= 12;
  const int check_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;
  const int check_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;
  
  if(check_op && check_x && check_y) {
    return Rf_ScalarLogical(1);
  }
  
  return Rf_ScalarLogical(0);


}