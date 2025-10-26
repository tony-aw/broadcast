

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_v, rng = false)]]
SEXP rcpp_bcRel_b_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_VECTOR);
   UNPROTECT(1);
   return out;
}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_VECTOR);
   UNPROTECT(1);
   return out;
}


}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_ov, rng = false)]]
SEXP rcpp_bcRel_b_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_ORTHOVECTOR);
   UNPROTECT(1);
   return out;

}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_ORTHOVECTOR);
   UNPROTECT(1);
   return out;
}

}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_bv, rng = false)]]
SEXP rcpp_bcRel_b_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_BIG2VECTOR);
   UNPROTECT(1);
   return out;

}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_BIG2VECTOR);
   UNPROTECT(1);
   return out;
}

}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_d, rng = false)]]
SEXP rcpp_bcRel_b_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_DOCALL);
   UNPROTECT(1);
   return out;
}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_DOCALL);
   UNPROTECT(1);
   return out;
}


}


