#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_bind_which_comnames ( SEXP out_dim, SEXP start, SEXP obj_dim ) {


int n = Rf_length(obj_dim);
int start0 = INTEGER(start)[0] - 1;

const int *pout_dim = INTEGER_RO(out_dim);
const int *pobj_dim = INTEGER_RO(obj_dim);

int counter = 0;

for(int i = 0; i < n; ++i) {
  if(pout_dim[i + start0] == pobj_dim[i]) {
    counter++;
  }
}

SEXP out = PROTECT(Rf_allocVector(INTSXP, counter));

if(counter > 0) {
  counter = 0;
  int *pout = INTEGER(out);
  for(int i = 0; i < n; ++i) {
    if(pout_dim[i + start0] == pobj_dim[i]) {
      pout[counter] = i + 1;
      counter++;
    }
  }
}



UNPROTECT(1);
return out;

}