

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_dec_v)]]
SEXP rcpp_bcDist_dec_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op, double prec
) {

double tempcalc;
int tempout;

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_DEC_DIST(MACRO_DIM_VECTOR);


UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_dec_ov)]]
SEXP rcpp_bcDist_dec_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op, double prec
) {

double tempcalc;
int tempout;

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_DEC_DIST(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_dec_d)]]
SEXP rcpp_bcDist_dec_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op, double prec
) {


double tempcalc;
int tempout;

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_DEC_DIST(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


