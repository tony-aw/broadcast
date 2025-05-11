#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_serial ( SEXP x ) {

double maximum = pow(2, 31) - 1;
double len = Rf_xlength(x);
double oversized = len - maximum;

double *pres;
SEXP res = PROTECT(Rf_allocVector(REALSXP, 4));
pres = REAL(res);

pres[0] = len;
pres[1] = oversized;
pres[2] = 0;
pres[3] = 202326021992;

UNPROTECT(1);

return(res);

}
