#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_make_outdim ( SEXP x_dim, SEXP y_dim, SEXP x_ndim, SEXP y_ndim ) {

  
  const int *restrict px_ndim = INTEGER_RO(x_ndim);
  const int *restrict py_ndim = INTEGER_RO(y_ndim);
  
  if(px_ndim[0] == 0 && py_ndim[0] == 0) {
    return R_NilValue;
  }
  else if(px_ndim[0] != 0 && py_ndim[0] != 0) {
    int n = py_ndim[0] > px_ndim[0] ? py_ndim[0] : py_ndim[0];
    const int *restrict px = INTEGER_RO(x_dim);
    const int *restrict py = INTEGER_RO(y_dim);
    SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
    int *pout = INTEGER(out);
    R_xlen_t out_len = 1;
    
    for(int i = 0; i < n; ++i) {
      if(py[i] > px[i]) {
        pout[i] = py[i];
      }
      else {
        pout[i] = px[i];
      }
      out_len *= pout[i];
    }
    
    const double maxlong = pow(2, 52);
    if(out_len >= maxlong) {
      error("broadcasting will exceed maximum size");
    }
    
    UNPROTECT(1);
    return(out);
  }
  else if(px_ndim[0] != 0) {
    return x_dim;
  }
  else {
    return y_dim;
  }
  
  return R_NilValue;

}