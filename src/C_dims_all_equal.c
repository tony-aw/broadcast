#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_dims_all_equal ( SEXP xdim, SEXP ydim ) {


 const int *pxdim = INTEGER_RO(xdim);
 const int *pydim = INTEGER_RO(ydim);
 int n = Rf_length(xdim);

 for(int i = 0; i < n; ++i) {
  if(pxdim[i] != pydim[i]) {
    return Rf_ScalarLogical(0);;
  }
 }
 
 return Rf_ScalarLogical(1);
 
}