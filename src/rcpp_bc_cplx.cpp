

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;





inline Rcomplex inline_cplx_plus( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  out.r = x.r + y.r;
  out.i = x.i + y.i;
  return out;
}


inline Rcomplex inline_cplx_min( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  out.r = x.r - y.r ;
  out.i = x.i - y.i ;
  return out;
}


inline Rcomplex inline_cplx_mult( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
  
  out.r = x.r * y.r - x.i * y.i;
  out.i = x.r * y.i + y.r * x.i;
  return out;
}



inline Rcomplex inline_cplx_div( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
  
  
  double ratio, den;
  double abr, abi;

  if( (abr = y.r) < 0) abr = - abr;
  if( (abi = y.i) < 0) abi = - abi;
  if( abr <= abi ) {
    ratio = y.r / y.i ;
    den = y.i * (1 + ratio*ratio);
    out.r = (x.r*ratio + x.i) / den;
    out.i = (x.i*ratio - x.r) / den;
  }
  else {
    ratio = y.i / y.r ;
    den = y.r * (1 + ratio*ratio);
    out.r = (x.r + x.i*ratio) / den;
    out.i = (x.i - x.r*ratio) / den;
  }
  return out;

}



#define MACRO_OP_CPLX_MATH(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_plus(tempx, tempy), \
      const Rcomplex tempx = px[flatind_x], \
      const Rcomplex tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_min(tempx, tempy), \
      const Rcomplex tempx = px[flatind_x], \
      const Rcomplex tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 3:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_mult(tempx, tempy), \
      const Rcomplex tempx = px[flatind_x], \
      const Rcomplex tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 4:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = inline_cplx_div(tempx, tempy), \
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
  // [[Rcpp::export(.rcpp_bc_cplx_2v, rng = false)]]
  SEXP rcpp_bc_cplx_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  ComplexVector out(no_init(nout));
  
  
Rcomplex *pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

  
  MACRO_OP_CPLX_MATH(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_cplx_d, rng = false)]]
  SEXP rcpp_bc_cplx_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  ComplexVector out(no_init(nout));
  
  
Rcomplex *pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

  
  MACRO_OP_CPLX_MATH(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  