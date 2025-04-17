#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_dropdims_which ( SEXP xdim, SEXP ydim, SEXP size ) {


 const int *pxdim = INTEGER_RO(xdim);
 const int *pydim = INTEGER_RO(ydim);
 int n = Rf_length(xdim);
 int size0 = INTEGER(size)[0];
 
 SEXP out = PROTECT(Rf_allocVector(INTSXP, size0));
 int *pout = INTEGER(out);
 int counter = 0;
 
 for(int i = 0; i < n; ++i) {
  if(pxdim[i] == 1 && pydim[i] == 1) {
    pout[counter] = i + 1;
    counter++;
  }
 }
 
 UNPROTECT(1);
 
 return out;
 

}