

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;





inline bool rcpp_int53_need_guard1(
  SEXP x, SEXP y
) {
  if(TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
    if(TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP) {
      return false;
    }
  }
  return true;
}


inline bool rcpp_int53_need_guard2(
  SEXP x, SEXP y
) {
  if(TYPEOF(x) == INTSXP && TYPEOF(y) == LGLSXP) {
    return false;
  }
  if(TYPEOF(x) == LGLSXP && TYPEOF(y) == INTSXP) {
    return false;
  }
  return true;
}


inline double rcpp_int53_guard(
  double out, double intmin, double intmax
) {
  if(out > intmax) {
    return R_PosInf;
  }
  if(out < intmin) {
    return R_NegInf;
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_gcd_rec)]]
long long rcpp_gcd_rec(
  long long x, long long y
) {

  long long a = abs(x);
  long long b = abs(y);
  
  if (a == 0) {
    return b;
  }
  if (b == 0) {
    return a;
  }
  if (a == b) {
    return a;
  }
  if (a > b) {
    return rcpp_gcd_rec(a - b, b);
  }
  return rcpp_gcd_rec(a, b - a);
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_gcd)]]
double rcpp_gcd(
  double x, double y
) {
  
  if(x == 0 && y == 0) {
    return NA_REAL;
  }
  else if(x == 0) {
    return y;
  }
  else if(y == 0) {
    return x;
  }
  
  return rcpp_gcd_rec((long long) x, (long long) y);
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_v)]]
SEXP rcpp_bc_int_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_ov)]]
SEXP rcpp_bc_int_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_bs)]]
SEXP rcpp_bc_int_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_BIGSMALL_DOCALL);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_d)]]
SEXP rcpp_bc_int_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


