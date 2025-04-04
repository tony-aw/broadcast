

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_v)]]
SEXP rcpp_bc_list_v(
  List x, List y, 
  R_xlen_t nout, Function f
) {


List out(nout);

MACRO_DIM_VECTOR(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_ov)]]
SEXP rcpp_bc_list_ov(
  List x, List y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, Function f
) {

List out(nout);

MACRO_DIM_ORTHOVECTOR(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);



return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_d)]]
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


