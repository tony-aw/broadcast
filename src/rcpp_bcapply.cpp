

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_v, rng = false)]]
void rcpp_bcapply_v(
  SEXP out, SEXP x, SEXP y, 
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_VECTOR);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_ov, rng = false)]]
void rcpp_bcapply_ov(
  SEXP out, SEXP x, SEXP y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_ORTHOVECTOR);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_bv, rng = false)]]
void rcpp_bcapply_bv(
  SEXP out, SEXP x, SEXP y,  bool bigx, SEXP out_dim,
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_BIG2VECTOR);

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_d, rng = false)]]
void rcpp_bcapply_d(
  SEXP out, SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, Function f
) {


MACRO_OP_BCAPPLY(MACRO_DIM_DOCALL);


}


