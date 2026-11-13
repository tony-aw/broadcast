

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;





inline int inline_cplx_equal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r == y.r && x.i == y.i) {
    return(1);
  }
  else return(0);
}


inline int inline_cplx_unequal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r != y.r || x.i != y.i) {
    return(1);
  }
  else return(0);
}



#define MACRO_OP_CPLX_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_equal(tempx, tempy),  \
      const Rcomplex tempx = px[flatind_x], \
      const Rcomplex tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_unequal(tempx, tempy),  \
      const Rcomplex tempx = px[flatind_x], \
      const Rcomplex tempy = py[flatind_y]  \
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
  // [[Rcpp::export(.rcpp_bcRel_cplx_2v, rng = false)]]
  SEXP rcpp_bcRel_cplx_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);
int *pout = LOGICAL(out);

  
  MACRO_OP_CPLX_REL(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_cplx_1v, rng = false)]]
  SEXP rcpp_bcRel_cplx_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);
int *pout = LOGICAL(out);

  
  MACRO_OP_CPLX_REL(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_cplx_d, rng = false)]]
  SEXP rcpp_bcRel_cplx_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  
const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);
int *pout = LOGICAL(out);

  
  MACRO_OP_CPLX_REL(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
