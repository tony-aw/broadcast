

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline SEXP inline_string_mult(
    String x, int y
  ) {
    if(x == NA_STRING || y < 0) {
      return(NA_STRING);
    }
    else if(y == 0) {
      String out = "";
      return out.get_sexp();
    }
    else if(y == 1) {
      return x.get_sexp();
    }
    else {
      String out = x;
      for(int i = 1; i < y; ++i) {
        out += x;
      }
      return out.get_sexp();
    }
}


#define MACRO_STRREP(DIMCODE) do { \
  DIMCODE(  \
    SET_STRING_ELT(out, flatind_out, inline_string_mult(tempx, tempy)), \
    const String tempx = px[flatind_x], \
    const int tempy = py[flatind_y] \
  );  \
} while(0)



  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_strrep_2v, rng = false)]]
  SEXP rcpp_bc_strrep_2v(
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
const int *py = INTEGER_RO(y);

  
  MACRO_STRREP(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_strrep_d, rng = false)]]
  SEXP rcpp_bc_strrep_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  StringVector out(no_init(nout));
  
  
const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

  
  MACRO_STRREP(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  