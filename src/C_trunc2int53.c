#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_trunc2int53 ( SEXP x ) {


  const R_xlen_t n = Rf_xlength(x);
  const double intmax = pow(2, 53) -1;
  const double intmin = -1 * intmax;
  
  const double *restrict px = REAL_RO(x);
  
  SEXP res = PROTECT(allocVector(REALSXP, n));
  double *restrict pres = REAL(res);
  
  for(int i = 0; i < n; ++i) {
    const double tempx = px[i];
    if(tempx > intmax) {
      pres[i] = R_PosInf;
    }
    else if(tempx < intmin) {
      pres[i] = R_NegInf;
    }
    else {
      pres[i] = trunc(tempx);
    }
  }
  
  UNPROTECT(1);
  return res;

}