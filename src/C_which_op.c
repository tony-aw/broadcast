#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_which_op ( SEXP list, SEXP op0 ) {
  
  const int n = Rf_length(list);
  const SEXP *plist = STRING_PTR_RO(list);
  const SEXP op = STRING_ELT(op0, 0);
  int res = 0;
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(plist[i], op, 0)) {
      res = i + 1;
      break;
    }
  }
  return Rf_ScalarInteger(res);
}