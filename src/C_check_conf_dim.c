#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_check_conf_dim ( SEXP xdim, SEXP ydim, SEXP xlen0, SEXP ylen0 ) {
    
  if(xdim == R_NilValue || ydim == R_NilValue) { // if x or y is NULL
    
    double xlen, ylen;
    
    if(TYPEOF(xlen0) == INTSXP) {
      xlen = (double) INTEGER(xlen0)[0];
    }
    else {
      xlen = REAL(xlen0)[0];
    }

    if(TYPEOF(ylen0) == INTSXP) {
      ylen = (double) INTEGER(ylen0)[0];
    }
    else {
      ylen = REAL(ylen0)[0];
    }
    
    if(xlen != ylen) {
      int out = xlen != 1 && ylen != 1;
      return(Rf_ScalarLogical(!out));
    }
    else {
      return(Rf_ScalarLogical(1));
   }
    
  } // END if x or y is NULL
  else {
    
    const int *pxdim = INTEGER_RO(xdim);
    const int *pydim = INTEGER_RO(ydim);
    int n = Rf_length(xdim);
    for(int i = 0; i < n; ++i) {
      if(pxdim[i] != 1 && pydim[i] != 1) {
        if(pxdim[i] != pydim[i]) {
           return Rf_ScalarLogical(0);
        }
      }
    }
    return Rf_ScalarLogical(1);
  }

}