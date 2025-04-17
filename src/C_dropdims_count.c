#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_dropdims_count ( SEXP xdim, SEXP ydim ) {


 const int *pxdim = INTEGER_RO(xdim);
 const int *pydim = INTEGER_RO(ydim);
 int n = Rf_length(xdim);
 
 int counter = 0;
 for(int i = 0; i < n; ++i) {
  if(pxdim[i] == 1 && pydim[i] == 1) {
    counter++;
  }
 }
 
 return Rf_ScalarInteger(counter);

}