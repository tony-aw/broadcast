#include <Rcpp/Lightest>
using namespace Rcpp;


inline SEXP rcpp_mergedims_output(
  SEXP x, SEXP y, int len
) {
  
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP xout = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP yout = PROTECT(Rf_allocVector(INTSXP, len));
  
  const int *px = INTEGER_RO(x);
  const int *py = INTEGER_RO(y);
  int *pxout = INTEGER(xout);
  int *pyout = INTEGER(yout);
  
  for(int i = 0; i < len; ++i) {
    pxout[i] = px[i];
    pyout[i] = py[i];
  }
  
  SET_VECTOR_ELT(out, 0, xout);
  SET_VECTOR_ELT(out, 1, yout);
  
  UNPROTECT(3);
  return out;
  
  
}


inline double rcpp_mergedims_prod(
  SEXP x, SEXP mergeable, int pos
) {
  
  const int *px = INTEGER_RO(x);
  const int *pmerge = LOGICAL_RO(mergeable);
  double res = 1;
  
  for(int i = pos; i >= 0; --i) {
    res *= px[i];
    if(!pmerge[i]) {
      return res;
    }
  }
  
  return res;
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_is_mergeable_with_prev)]]
SEXP rcpp_is_mergeable_with_prev(
  SEXP xB, SEXP yB
) {
  
  // xD = x.dims == 1L
  // yD = y.dims == 1L
  
  
  // point to arguments:
  const int *pxB = INTEGER_RO(xB);
  const int *pyB = INTEGER_RO(yB);
  
  // make new objects:
  int n = Rf_length(xB);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *pout = LOGICAL(out);
  
  // assign for first iteration:
  int mergeable;
  pout[0] = 1;
  
  // start loop:
  for(int i = 1; i < n; ++i) {
    mergeable = 0;
    
    if(pxB[i] == pxB[i - 1] && pyB[i] == pyB[i - 1]) {
      mergeable = 1;
    }
    
    pout[i] = mergeable;
  }
  // end loop
  
  UNPROTECT(1);
  return out;
  
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_mergedims)]]
SEXP rcpp_mergedims(
  SEXP xD, SEXP yD, SEXP mergeable
) {

  // xD = x.dims
  // yD = y.dims


  // point to arguments:
  const int *pxD = INTEGER_RO(xD);
  const int *pyD = INTEGER_RO(yD);
  const int *pmerg = LOGICAL_RO(mergeable);


  // make new objects:
  double intmax = pow(2, 31) - 1;
  int n = Rf_length(xD);
  SEXP xout = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP yout = PROTECT(Rf_allocVector(INTSXP, n));
  int *pxout = INTEGER(xout);
  int *pyout = INTEGER(yout);
  int counter = 0;
  double temp_xprod;
  double temp_yprod;
  

  // assign for first iteration:
  pxout[0] = pxD[0];
  pyout[0] = pyD[0];
  bool check;

  // start loop:
  for(int i = 1; i < n; ++i) {
    
    temp_xprod = rcpp_mergedims_prod(xD, mergeable, i);
    temp_yprod = rcpp_mergedims_prod(yD, mergeable, i);
    check = temp_xprod < intmax && temp_yprod < intmax;
    
    if(pmerg[i] && check) {
      pxout[counter] = (int)temp_xprod;
      pyout[counter] = (int)temp_yprod;
    }
    else {
      counter++;
      pxout[counter] = pxD[i];
      pyout[counter] = pyD[i];
    }
  }
  // end loop

  // create & return output:
  SEXP out = rcpp_mergedims_output(xout, yout, counter + 1);
  UNPROTECT(2);
  return out;

}

