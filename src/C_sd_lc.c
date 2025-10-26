#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sd_lc(
  SEXP w, SEXP vcdiag, SEXP vc, SEXP nobs0, SEXP nvars0, SEXP bad_rp0
) {
  
  // note: x[a, b] == x[a + b * nobs]
  
  int nvars = INTEGER(nvars0)[0];
  int nobs = INTEGER(nobs0)[0];
  double bad_rp = REAL(bad_rp0)[0];
  const double *pvc = REAL_RO(vc);
  const double *pvcdiag = REAL_RO(vcdiag);
  double var, covar;
  R_xlen_t i_t_nvars;
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, nobs));
  double *pout = REAL(out);
  
  if(TYPEOF(w) == REALSXP) { // start decimal function
    const double *pw = REAL_RO(w);
    double weight;
    for(int k = 0; k < nobs; ++k) {
      
      var = 0.0;
      covar = 0.0;
      for(int i = 0; i < nvars; ++i) {
        weight = pw[k + i * nobs];
        i_t_nvars = i * nvars;
        var += pvcdiag[i] * weight * weight;
        for(int j = (i + 1); j < nvars; ++j) {
          covar += pw[k + j * nobs] * weight * pvc[j + i_t_nvars];
        }
      }
            
      var = var + 2 * covar;
      if(var < 0) {
        pout[k] = bad_rp;
      }
      else {
        pout[k] = sqrt(var);
      }
      
    }
  } // end decimal function
  
  if(TYPEOF(w) == INTSXP || TYPEOF(w) == LGLSXP) { // start integer function
    int isNA;
    const int *pw = INTEGER_RO(w);
    int weight;
    
    for(int k = 0; k < nobs; ++k) {
      
      // NAs:
      isNA = 0;
      for(int i = 0; i < nvars; ++i) {
        if(pw[k + i * nobs] == NA_INTEGER) {
          isNA = 1;
          break;
        }
      }
      if(isNA) {
        // return NA
        pout[k] = NA_REAL;
      }
      else { // start regular integer computation
        var = 0.0;
        covar = 0.0;
        for(int i = 0; i < nvars; ++i) {
          weight = pw[k + i * nobs];
          i_t_nvars = i * nvars;
          var += pvcdiag[i] * weight * weight;
          for(int j = (i + 1); j < nvars; ++j) {
            covar += pw[k + j * nobs] * weight * pvc[j + i_t_nvars];
          }
        }

        var = var + 2 * covar;
        if(var < 0) {
          pout[k] = bad_rp;
        }
        else {
          pout[k] = sqrt(var);
        }
      } // end regular integer computation
    }
  } // end integer function
  
  
  
  UNPROTECT(1);
  return out;
  
}