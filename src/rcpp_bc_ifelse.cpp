

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_cplx_returnNA)]]
Rcomplex rcpp_cplx_returnNA() {
    Rcomplex out;
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_v)]]
SEXP rcpp_bc_ifelse_v(
  SEXP cond, SEXP x, SEXP y,
  R_xlen_t nout
) {

const int *pcond = INTEGER(cond);

MACRO_OP_IFELSE(
  MACRO_DIM_VECTOR
);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_ov)]]
SEXP rcpp_bc_ifelse_ov(
  SEXP cond, SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout
) {

const int *pcond = INTEGER(cond);

MACRO_OP_IFELSE(
  MACRO_DIM_ORTHOVECTOR
);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_bs)]]
SEXP rcpp_bc_ifelse_bs(
  SEXP cond, SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx
) {

const int *pcond = INTEGER(cond);

MACRO_OP_IFELSE(
  MACRO_DIM_BIGSMALL_DOCALL
);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_d)]]
SEXP rcpp_bc_ifelse_d(
  SEXP cond, SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout
) {

const int *pcond = INTEGER(cond);

MACRO_OP_IFELSE(
  MACRO_DIM_DOCALL
);

}


