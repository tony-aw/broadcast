#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sd_gauss_lc(
  SEXP w, SEXP vc, SEXP nvars0, SEXP nobs0, SEXP bad_rp0
) {
  
  
  int nvars = INTEGER(nvars0)[0];
  int nobs = INTEGER(nobs0)[0];
  double bad_rp = REAL(bad_rp0)[0];
  
  const double *pw = REAL_RO(w);
  const double *pvc = REAL_RO(vc);
  double sum1, sum2;
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, nobs));
  double *pout = REAL(out);

  for(int k = 0; k < nobs; ++k) {
    sum2 = 0.0;
    for(int i = 0; i < nvars; ++i) {
      sum1 = 0.0;
      for(int j = 0; j < nvars; ++j) {
        sum1 += pw[k + j * nobs] * pvc[j + i * nvars];
      }
      sum2 += sum1 * pw[k + i * nobs];
    }
    if(sum2 < 0) {
      pout[k] = bad_rp;
    }
    else {
      pout[k] = sqrt(sum2);
    }
  }
  
  UNPROTECT(1);
  return out;
  
}

