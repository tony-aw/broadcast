

#include <Rcpp/Lightest>
#include "Broadcast.h"

using namespace Rcpp;



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_string_plus)]]
String rcpp_string_plus(
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
// [[Rcpp::export(.rcpp_bc_str_v)]]
SEXP rcpp_bc_str_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);
MACRO_OP_STR_CONC(
  MACRO_DIM_VECTOR
);


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_ov)]]
SEXP rcpp_bc_str_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_ORTHOVECTOR
);


return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_bs)]]
SEXP rcpp_bc_str_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {

CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_BIGSMALL_DOCALL
);

return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_d)]]
SEXP rcpp_bc_str_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_DOCALL
);

return out;

}


