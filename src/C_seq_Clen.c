#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_seq_Clen ( SEXP start, SEXP len ) {

    
  if(TYPEOF(len) == REALSXP && TYPEOF(start) == REALSXP) {
    const R_xlen_t len0 = REAL_RO(len)[0];
    const R_xlen_t start0 = REAL_RO(start)[0];
    SEXP out = PROTECT(Rf_allocVector(REALSXP, len0));
    double *pout = REAL(out);
  
    for(int i = 0; i < len0; ++i) {
      pout[i] = start0 + i;
    }
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(len) == INTSXP && TYPEOF(start) == INTSXP) {
    const int len0 = INTEGER_RO(len)[0];
    const int start0 = INTEGER_RO(start)[0];
    SEXP out = PROTECT(Rf_allocVector(INTSXP, len0));
    int *pout = INTEGER(out);
  
    for(int i = 0; i < len0; ++i) {
      pout[i] = start0 + i;
    }
    
    UNPROTECT(1);
    return out;
  }
  else {
    error("unsupported types given");
  }
  

}