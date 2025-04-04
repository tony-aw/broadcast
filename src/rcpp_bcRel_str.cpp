

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_v)]]
SEXP rcpp_bcRel_str_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_VECTOR);


UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_ov)]]
SEXP rcpp_bcRel_str_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {


int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_d)]]
SEXP rcpp_bcRel_str_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


