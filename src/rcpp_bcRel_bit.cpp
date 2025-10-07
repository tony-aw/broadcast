

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline Rbyte rcpp_bit_se_raw(Rbyte x, Rbyte y) {
  return (~x & y) | (x & y) | (~x & ~y);
}

inline Rbyte rcpp_bit_ge_raw(Rbyte x, Rbyte y) {
  return (x & ~y) | (x & y) | (~x & ~y);
}

inline int rcpp_bit_se_int(int x, int y) {
  return (~x & y) | (x & y) | (~x & ~y);
}

inline int rcpp_bit_ge_int(int x, int y) {
  return (x & ~y) | (x & y) | (~x & ~y);
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_v)]]
SEXP rcpp_bcRel_bit_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_VECTOR);
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_VECTOR);
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop("unsupported combinations of types given");
  }

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_ov)]]
SEXP rcpp_bcRel_bit_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {


  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_ORTHOVECTOR);
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_ORTHOVECTOR);
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop("unsupported combinations of types given");
  }


}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_d)]]
SEXP rcpp_bcRel_bit_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_DOCALL);
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_DOCALL);
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop("unsupported combinations of types given");
  }


}


