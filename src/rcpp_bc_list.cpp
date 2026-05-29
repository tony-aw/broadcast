

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_v, rng = false)]]
SEXP rcpp_bc_list_v(
  List x, List y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, Function f
) {


List out(nout);

MACRO_DIM_VECTORSPECIAL(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_d, rng = false)]]
SEXP rcpp_bc_list_d(
  List x, List y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, Function f
) {


List out(nout);

MACRO_DIM_DOCALL(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);

return out;

}


