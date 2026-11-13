
#include <Rcpp/Lightest>
#include "broadcast.h"
using namespace Rcpp;

  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_2v, rng = false)]]
List rcpp_bc_list_2v(
  List x, List y, List prep, Function f
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;


List out(no_init(nout));


MACRO_DIM_TWOVECTORS(
  out[flatind_out] = f(tempx, tempy),
  SEXP tempx = x[flatind_x],
  SEXP tempy = y[flatind_y]
);

out.attr("dim") = out_dimorig;

return out;

}



  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_1v, rng = false)]]
List rcpp_bc_list_1v(
  List x, List y, List prep, Function f
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;


List out(no_init(nout));


MACRO_DIM_ONEVECTOR(
  out[flatind_out] = f(tempx, tempy),
  SEXP tempx = x[flatind_x],
  SEXP tempy = y[flatind_y]
);

out.attr("dim") = out_dimorig;

return out;

}



  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_d, rng = false)]]
List rcpp_bc_list_d(
  List x, List y, List prep, Function f
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;


List out(no_init(nout));


MACRO_DIM_DOCALL(
  out[flatind_out] = f(tempx, tempy),
  SEXP tempx = x[flatind_x],
  SEXP tempy = y[flatind_y]
);

out.attr("dim") = out_dimorig;

return out;

}


