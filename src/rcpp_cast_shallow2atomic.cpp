
#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;


inline bool rcpp_unlisthelper_is_atomic(SEXP x) {
  switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    {
      return TRUE;
    }
    default:
    {
      return FALSE;
    }
  }
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_unlisthelper_max_type)]]
int rcpp_unlisthelper_max_type(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  int out = 1;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    if(TYPEOF(tempout) == VECSXP || TYPEOF(tempout) == LISTSXP) {
      stop("`x` must be a shallow list");
    }
    if(!rcpp_unlisthelper_is_atomic(tempout)) {
      return 9;
    }
    else if(TYPEOF(tempout) == STRSXP && out < 9) {
      out = 7;
    }
    else if(TYPEOF(tempout) == CPLXSXP && out < 7) {
      out = 6;
    }
    else if(TYPEOF(tempout) == REALSXP && out < 6) {
      out = 5;
    }
    else if(TYPEOF(tempout) == INTSXP && out < 5) {
      out = 4;
    }
    else if(TYPEOF(tempout) == LGLSXP && out < 4) {
      out = 3;
    }
    else if(TYPEOF(tempout) == RAWSXP && out < 3) {
      out = 2;
    }
    else if(out < 2){
      out = 1;
    }
  }
  
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_shallow2atomic)]]
void rcpp_shallow2atomic(
    SEXP input, SEXP output, int nrow, int ncol, int arrangement
) {
  R_xlen_t flatind = 0;
  switch(TYPEOF(output)) {
    case LGLSXP:
    case INTSXP:
    {
      int *pout = INTEGER(output);
      if(arrangement == -1) {
        MACRO_UNLIST_DEEP(const int *pinput = INTEGER_RO(tempin), pout[flatind] = pinput[j]);
      }
      else if(arrangement == 1) {
        MACRO_UNLIST_DOWN(const int *pinput = INTEGER_RO(tempin), pout[flatind] = pinput[j]);
      }
      break;
    }
    case REALSXP:
    {
      double *pout = REAL(output);
      if(arrangement == -1) {
        MACRO_UNLIST_DEEP(const double *pinput = REAL_RO(tempin), pout[flatind] = pinput[j]);
      }
      else if(arrangement == 1) {
        MACRO_UNLIST_DOWN(const double *pinput = REAL_RO(tempin), pout[flatind] = pinput[j]);
      }
      break;
    }
    case CPLXSXP:
    {
      Rcomplex *pout = COMPLEX(output);
      if(arrangement == -1) {
        MACRO_UNLIST_DEEP(const Rcomplex *pinput = COMPLEX_RO(tempin), pout[flatind] = pinput[j]);
      }
      else if(arrangement == 1) {
        MACRO_UNLIST_DOWN(const Rcomplex *pinput = COMPLEX_RO(tempin), pout[flatind] = pinput[j]);
      }
      break;
    }
    case STRSXP:
    {
      if(arrangement == -1) {
        MACRO_UNLIST_DEEP(const SEXP *pinput = STRING_PTR_RO(tempin), SET_STRING_ELT(output, flatind, pinput[j]));
      }
      else if(arrangement == 1) {
        MACRO_UNLIST_DOWN(const SEXP *pinput = STRING_PTR_RO(tempin), SET_STRING_ELT(output, flatind, pinput[j]));
      }
      break;
    }
    case RAWSXP:
    {
      Rbyte *pout = RAW(output);
      if(arrangement == -1) {
        MACRO_UNLIST_DEEP(const Rbyte *pinput = RAW_RO(tempin), pout[flatind] = pinput[j]);
      }
      else if(arrangement == 1) {
        MACRO_UNLIST_DOWN(const Rbyte *pinput = RAW_RO(tempin), pout[flatind] = pinput[j]);
      }
      break;
    }
    default:
    {
      stop("unknown type given");
    }
  }
}
