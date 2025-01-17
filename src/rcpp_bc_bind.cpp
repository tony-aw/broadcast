

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(rcpp_bc_bind)]]
void rcpp_bc_bind(
  SEXP out, SEXP x,
  SEXP starts, SEXP ends, SEXP by_x,
  SEXP dimcumprod_out, SEXP dimcumprod_x, SEXP out_dim
) {


double *pout = REAL(out);
double *px = REAL(x);

MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);

}


