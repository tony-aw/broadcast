

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;



#define MACRO_OP_RAW_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx == tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx != tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
    );                                                                \
    break;	\
  }	\
  case 3:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx < tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
    );                                                                \
    break;	\
  }	\
  case 4:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx > tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
    );                                                                \
    break;	\
  }	\
  case 5:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx <= tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
    );                                                                \
    break;	\
  }	\
  case 6:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = tempx >= tempy, \
      const Rbyte tempx = px[flatind_x],  \
      const Rbyte tempy = py[flatind_y] \
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
  // [[Rcpp::export(.rcpp_bcRel_raw_2v, rng = false)]]
  SEXP rcpp_bcRel_raw_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

  
  MACRO_OP_RAW_REL(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_raw_1v, rng = false)]]
  SEXP rcpp_bcRel_raw_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

  
  MACRO_OP_RAW_REL(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_raw_d, rng = false)]]
  SEXP rcpp_bcRel_raw_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

  
  MACRO_OP_RAW_REL(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
