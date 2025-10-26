
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_determine_dimmode( SEXP xdim, SEXP ydim, SEXP xlen0, SEXP ylen0 ) {
  
  // Get Properties ============================================================
  
  double xlen, ylen;
  if(TYPEOF(xlen0) == INTSXP) {
    xlen = (double) INTEGER(xlen0)[0];
  }
  else {
    xlen = REAL(xlen0)[0];
  }
  
  if(TYPEOF(ylen0) == INTSXP) {
    ylen = (double) INTEGER(ylen0)[0];
  }
  else {
    ylen = REAL(ylen0)[0];
  }
  
  const int xndim = Rf_length(xdim);
  const int yndim = Rf_length(ydim);
  
  
  // Use Vector mode ===========================================================
  
  if(xlen == 1L || ylen == 1L) { // x and/or y are/is scalar(s)
    return(Rf_ScalarInteger(1));
  }
  else if(xndim <= 1 && yndim <= 1) { // x and y are vectors or 1d arrays
    return(Rf_ScalarInteger(1));
  }
  
  const int ndim = xndim;
  const int *pxdim = INTEGER_RO(xdim);
  const int *pydim = INTEGER_RO(ydim);
  int dims_all_equal = 1;
  for(int i = 0; i < ndim; ++i) {
    if(pxdim[i] != pydim[i]) {
      dims_all_equal = 0;
      break;
    }
  }
  if(dims_all_equal) { // x and y are arrays of equal dimensions
    return(Rf_ScalarInteger(1));
  }
  
  
  
  // Use OrthoVector mode ======================================================
  
  if(ndim == 2) {
    int check1 = pxdim[0] != pydim[0] && pxdim[1] != pydim[1];
    int check2 = pxdim[0] != pxdim[1] && pydim[0] != pydim[1];
    int check3 = pxdim[0] == 1 || pxdim[1] == 1 || pydim[0] == 1 || pydim[1] == 1;
    if(check1 && check2 && check3) {
      return(Rf_ScalarInteger(2));
    }
  }
  
  
  
  // Use Big2Vector mode =======================================================
  
  // IF merging of dimensions is successful, a big array by vector mode can only come in 2 forms:
  //  - A matrix with a vector at end a la c(1, n)/c(n, 1);
  //  - a 3d array with a sandwiched vector a la c(1, n, 1).

  if(ndim == 2) {
    // If ndim == 2, and it's not of mode vector or orhtovector, it can only be of mode big2vector
    return(Rf_ScalarInteger(3));
  }
  
  if( ndim == 3 ) {
    // for 3 dimensions, the big2vector MACRO I made only supports sandwiched vectors, so this must be checked carefully
    
    int sandwichx = (pxdim[0] == 1) && (pxdim[1] > 1) && (pxdim[2] == 1);
    int sandwichy = (pydim[0] == 1) && (pydim[1] > 1) && (pydim[2] == 1);
    
    if(sandwichx || sandwichy) {
      
      int bigx = (pxdim[0] >= pydim[0]) && (pxdim[1] >= pydim[1]) && (pxdim[2] >= pydim[2]);
      int bigy = (pydim[0] >= pxdim[0]) && (pydim[1] >= pxdim[1]) && (pydim[2] >= pxdim[2]);
      
      if(bigx || bigy) {
        return(Rf_ScalarInteger(3));
      }
    }
  }
  
  
  
  // Use General Mode ==========================================================
  return(Rf_ScalarInteger(4));
  
  
  warning("your C program does not return anything!");
  return R_NilValue;
}