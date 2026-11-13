

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;



#define MACRO_ACTION_BOOL_RAW(DIMCODE, DOCODE) do {     \
  DIMCODE(                                              \
    pout[flatind_out] = DOCODE,                         \
    const Rbyte tempx = px[flatind_x],                  \
    const Rbyte tempy = py[flatind_y]                   \
  );                                                    \
} while(0)



#define MACRO_OP_BOOL_RAW(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, tempx && tempy);  \
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, tempx || tempy);  \
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx != (bool)tempy);  \
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, !(tempx && tempy));  \
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, !(tempx || tempy));  \
      break;	\
    }	\
    case 6:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx == (bool)tempy);  \
      break;	\
    }	\
    case 7:	\
    {	\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx != (bool)tempy);  \
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
  // [[Rcpp::export(.rcpp_bc_b_raw_2v, rng = false)]]
  SEXP rcpp_bc_b_raw_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  RawVector out(no_init(nout));
  
  
const Rbyte *px = RAW_RO(x);
const Rbyte *py = RAW_RO(y);
Rbyte *pout = RAW(out);

  
  MACRO_OP_BOOL_RAW(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_b_raw_1v, rng = false)]]
  SEXP rcpp_bc_b_raw_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  RawVector out(no_init(nout));
  
  
const Rbyte *px = RAW_RO(x);
const Rbyte *py = RAW_RO(y);
Rbyte *pout = RAW(out);

  
  MACRO_OP_BOOL_RAW(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_b_raw_d, rng = false)]]
  SEXP rcpp_bc_b_raw_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  RawVector out(no_init(nout));
  
  
const Rbyte *px = RAW_RO(x);
const Rbyte *py = RAW_RO(y);
Rbyte *pout = RAW(out);

  
  MACRO_OP_BOOL_RAW(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
