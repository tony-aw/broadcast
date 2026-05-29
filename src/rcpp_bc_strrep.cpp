

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline String rcpp_string_mult(
    String x, int y
  ) {
    if(x == NA_STRING || y < 0) {
      return(NA_STRING);
    }
    else if(y == 0) {
      return "";
    }
    else if(y == 1) {
      return x;
    }
    else {
      String out = x;
      for(int i = 1; i < y; ++i) {
        out += x;
      }
      return(out);
    }
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_v, rng = false)]]
SEXP rcpp_bc_strrep_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx
) {

const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);
MACRO_DIM_VECTORSPECIAL(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_d, rng = false)]]
SEXP rcpp_bc_strrep_d(
  SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout
) {


const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);

MACRO_DIM_DOCALL(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));

return out;

}


