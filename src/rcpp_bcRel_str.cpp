

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline int inline_str_eq(SEXP x, SEXP y) {
  if(x == NA_STRING || y == NA_STRING) {
    return NA_LOGICAL;
  }
  else {
    return (int)R_compute_identical(x, y, 0);
  }
}


inline int inline_str_uneq(SEXP x, SEXP y) {
  if(x == NA_STRING || y == NA_STRING) {
    return NA_LOGICAL;
  }
  else {
    return (int)!R_compute_identical(x, y, 0);
  }
}


#define MACRO_OP_STR_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_str_eq(tempx, tempy),  \
      const SEXP tempx = px[flatind_x], \
      const SEXP tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_str_uneq(tempx, tempy),  \
      const SEXP tempx = px[flatind_x], \
      const SEXP tempy = py[flatind_y]  \
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
  // [[Rcpp::export(.rcpp_bcRel_str_2v, rng = false)]]
  SEXP rcpp_bcRel_str_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout = LOGICAL(out);


  
  MACRO_OP_STR_REL(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_str_1v, rng = false)]]
  SEXP rcpp_bcRel_str_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout = LOGICAL(out);


  
  MACRO_OP_STR_REL(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_str_d, rng = false)]]
  SEXP rcpp_bcRel_str_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout = LOGICAL(out);


  
  MACRO_OP_STR_REL(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
