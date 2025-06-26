#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_recycle_seq_dim ( SEXP x, SEXP y ) {


const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);

int n = Rf_length(x);
SEXP out = PROTECT(Rf_allocVector(VECSXP, n));



for(int i = 0; i < n; ++i) {
  int size = py[i];
  SEXP temp = PROTECT(Rf_allocVector(INTSXP, size));
  int *ptemp = INTEGER(temp);
  int counter = 1;
  for(int j = 0; j < size; ++j) {
    ptemp[j] = counter;
    counter++;
    if(counter > px[i]) {
      counter = 1;
    }
  }
  SET_VECTOR_ELT(out, i, temp);
  UNPROTECT(1);
  
}

UNPROTECT(1);

return out;


}