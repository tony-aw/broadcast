

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;



inline bool rcpp_isTRUE(
  int x
) {
  return(x != NA_INTEGER && x != 0);
}

inline bool rcpp_isFALSE(
  int x
) {
  return(x != NA_INTEGER && x == 0);
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_v)]]
SEXP rcpp_bc_b_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_ov)]]
SEXP rcpp_bc_b_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_bs)]]
SEXP rcpp_bc_b_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_BIGSMALL_DOCALL);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_d)]]
SEXP rcpp_bc_b_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


