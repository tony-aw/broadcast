

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;



inline Rcomplex rcpp_cplx_returnNA() {
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


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_VECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_VECTOR
    );
  }
  else {
    stop("unsupported type given");
  }


}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_ov)]]
SEXP rcpp_bc_ifelse_ov(
  SEXP cond, SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout
) {


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_ORTHOVECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_ORTHOVECTOR
    );
  }
  else {
    stop("unsupported type given");
  }

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


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_DOCALL
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_DOCALL
    );
  }
  else {
    stop("unsupported type given");
  }

}


