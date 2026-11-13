

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline SEXP inline_string_plus(
    String x, String y
  ) {
    if(x == NA_STRING || y == NA_STRING) {
      return(NA_STRING);
    }
    else {
      String out = x;
      out += y;
      return(out.get_sexp());
    }
}


#define MACRO_OP_STR_PLUS(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      SET_STRING_ELT(out, flatind_out, inline_string_plus(tempx, tempy)),  \
      const String tempx = px[flatind_x], \
      const String tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)

  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_str_2v, rng = false)]]
  SEXP rcpp_bc_str_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  StringVector out(no_init(nout));
  
  
const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

  
  MACRO_OP_STR_PLUS(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_str_d, rng = false)]]
  SEXP rcpp_bc_str_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  StringVector out(no_init(nout));
  
  
const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

  
  MACRO_OP_STR_PLUS(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  