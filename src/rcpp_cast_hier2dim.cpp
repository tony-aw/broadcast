
#include <Rcpp/Lightest>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rec_cast_hier2dim)]]
void rcpp_rec_cast_hier2dim(
  SEXP x, SEXP out, SEXP dcp, int dimpart_prev, int depth, int depth_target
) {
  const double dcp_current = REAL_RO(dcp)[depth - 1];
  R_xlen_t dimpart_current;
    
  if(depth == depth_target) {
    for(R_xlen_t i = 0; i < Rf_xlength(x); ++i) {
      dimpart_current = i * dcp_current + dimpart_prev;
      SEXP temp = VECTOR_ELT(x, i);
      SET_VECTOR_ELT(out, dimpart_current, temp);
    }
  }
  else {
    for(R_xlen_t i = 0; i < Rf_xlength(x); ++i) {
      dimpart_current = i * dcp_current + dimpart_prev;
      SEXP temp = VECTOR_ELT(x, i);
      rcpp_rec_cast_hier2dim(temp, out, dcp, dimpart_current, depth + 1, depth_target);
    }
  }
}

