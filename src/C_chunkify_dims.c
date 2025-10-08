#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_chunkify_dims ( SEXP dims, SEXP chunks ) {

int chunksize = 0;
int ndim = Rf_length(dims);

int *pchunks = INTEGER(chunks);
for(int i = 0; i < Rf_length(chunks); ++i) {
  if(ndim <= pchunks[i]) {
    chunksize = pchunks[i];
    break;
  }
}

const int *pdims = INTEGER_RO(dims);
SEXP out = PROTECT(Rf_allocVector(INTSXP, chunksize));
int *pout = INTEGER(out);

for(int i = 0; i < ndim; ++i) {
  pout[i] = pdims[i];
}
for(int i = ndim; i < chunksize; ++i) {
  pout[i] = 1;
}

UNPROTECT(1);
return out;

}