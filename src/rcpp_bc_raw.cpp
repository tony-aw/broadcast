

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline Rbyte rcpp_raw_diff(Rbyte x, Rbyte y) {
  Rbyte out = (x > y) ? (x - y) : (y - x);
  return out;
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_v, rng = false)]]
SEXP rcpp_bc_raw_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_ov, rng = false)]]
SEXP rcpp_bc_raw_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_bv, rng = false)]]
SEXP rcpp_bc_raw_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_BIG2VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_d, rng = false)]]
SEXP rcpp_bc_raw_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


