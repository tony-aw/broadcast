#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_lst_ndims ( SEXP x ) {



if(TYPEOF(x) != VECSXP) {
  error("`x` must be a list");
}

int n = Rf_length(x);
SEXP tempout;
SEXP tempdim;
int tempndims;

SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
int *pout = INTEGER(out);

const SEXP *px = (SEXP *) DATAPTR_RO(x);

for(int i = 0; i < n; ++i) {
  tempout = px[i];
  tempdim = Rf_getAttrib(tempout, R_DimSymbol);
  tempndims = Rf_length(tempdim);
  pout[i] = tempndims;
}

UNPROTECT(1);
return out;

}