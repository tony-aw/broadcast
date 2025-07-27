#include <Rcpp/Lightest>
using namespace Rcpp;


inline SEXP rcpp_vector_elt(
  SEXP x, int i, int len
) {
  if(i >= len) {
    return R_NilValue;
  }
  else {
    return VECTOR_ELT(x, i);
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_make_dimnames2)]]
SEXP rcpp_make_dimnames2(
    SEXP xdimnames, SEXP ydimnames, SEXP outdim, int pref
  ) {
  
  
    int n = Rf_xlength(outdim);
    SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
    int *poutdim = INTEGER(outdim);
    bool checkx;
    bool checky;
    int lenx = Rf_length(xdimnames);
    int leny = Rf_length(ydimnames);
    
    
    for(int i = 0; i < n; ++i) {
    
      SEXP tempx = rcpp_vector_elt(xdimnames, i, lenx);
      SEXP tempy = rcpp_vector_elt(ydimnames, i, leny);
      checkx = tempx != R_NilValue && Rf_length(tempx) == poutdim[i];
      checky = tempy != R_NilValue && Rf_length(tempy) == poutdim[i];
      
      
      if(!checkx && !checky) {
        continue;
      }
      
      
      if(checkx && checky) {
        if(std::addressof(tempx) == std::addressof(tempy)) {
          SET_VECTOR_ELT(out, i, tempx);
        }
        else if(pref == 1) {
          SET_VECTOR_ELT(out, i, tempx);
        }
        else if(pref == 2) {
          SET_VECTOR_ELT(out, i, tempy);
        }
        else {
          continue;
        }
      }
      else if(checkx) {
        SET_VECTOR_ELT(out, i, tempx);
      }
      else if(checky) {
        SET_VECTOR_ELT(out, i, tempy);
      }
      
    }
    
    UNPROTECT(1);
    return out;
  }


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_make_dimnames1)]]
SEXP rcpp_make_dimnames1(
    SEXP y_dimnames, SEXP ydim, SEXP outdim
  ) {
  
    SEXP out = PROTECT(Rf_allocVector(VECSXP, Rf_length(outdim)));
  
    int *pydim = INTEGER(ydim);
    int *poutdim = INTEGER(outdim);
    
    for(int i = 0; i < Rf_length(ydim); ++i) {
      if(pydim[i] == poutdim[i]) {
        SEXP temp = VECTOR_ELT(y_dimnames, i);
        SET_VECTOR_ELT(out, i, temp);
      }
    }
    
    UNPROTECT(1);
    
    return out;
  }


