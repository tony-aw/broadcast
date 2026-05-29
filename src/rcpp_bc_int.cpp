

#include <Rcpp/Lightest>
#include "broadcast.h"

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
// [[Rcpp::export(.rcpp_bc_int_v, rng = false)]]
SEXP rcpp_bc_int_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_VECTORSPECIAL);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_d, rng = false)]]
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


