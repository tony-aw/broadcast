

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;




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


inline double rcpp_int53_mod(double x, double y, double intmin, double intmax) {
  if(x < intmin || x > intmax || y < intmin || y > intmax) {
    return NA_REAL;
  }
  if((long long) y == 0) {
    return NA_REAL;
  }
  double out = (long long) x % (long long) y;
  return out;
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


