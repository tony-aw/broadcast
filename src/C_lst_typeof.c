#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_lst_typeof ( SEXP x ) {


if(TYPEOF(x) != VECSXP) {
  error("`x` must be a list");
}

int n = Rf_length(x);
SEXP tempout;
SEXP tempstr;

SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
SEXP *pout = STRING_PTR(out);

for(int i = 0; i < n; ++i) {
  tempout = VECTOR_ELT(x, i);
  if(TYPEOF(tempout) == OBJSXP && !Rf_isS4(tempout)) {
    tempstr = mkString("object");
  }
  else {
    tempstr = type2str(TYPEOF(tempout));
  }
  pout[i] = tempstr;
}

UNPROTECT(1);
return out;

}