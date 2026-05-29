

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline String rcpp_string_plus(
    String x, String y
  ) {
    if(x == NA_STRING || y == NA_STRING) {
      return(NA_STRING);
    }
    else {
      String out = x;
      out += y;
      return(out);
    }
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_v, rng = false)]]
SEXP rcpp_bc_str_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

CharacterVector out(nout);
MACRO_OP_STR_PLUS(
  MACRO_DIM_VECTORSPECIAL
);


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_d, rng = false)]]
SEXP rcpp_bc_str_d(
  SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

CharacterVector out(nout);

MACRO_OP_STR_PLUS(
  MACRO_DIM_DOCALL
);

return out;

}


