#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_binames_consider_dim(SEXP outdims, SEXP xdims, SEXP xdimnames ) {

 int n = Rf_length(xdims);
 int *pxdims = INTEGER(xdims);
 int *poutdims = INTEGER(outdims);

 for(int i = 0; i < n; ++i) {
   if(pxdims[i] == poutdims[i] && VECTOR_ELT(xdimnames, i) != R_NilValue) {
     return Rf_ScalarLogical(1);
   }
 }
 return Rf_ScalarLogical(0);
}