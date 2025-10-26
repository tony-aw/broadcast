

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline Rcomplex rcpp_cplx_plus( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
  
  out.r = x.r + y.r;
  out.i = x.i + y.i;
  return out;
}


inline Rcomplex rcpp_cplx_min( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
  
  out.r = x.r - y.r ;
  out.i = x.i - y.i ;
  return out;
}


inline Rcomplex rcpp_cplx_mult( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
  
  out.r = x.r * y.r - x.i * y.i;
  out.i = x.r * y.i + y.r * x.i;
  return out;
}



inline Rcomplex rcpp_cplx_div( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
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
  return out ;

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_cplx_v, rng = false)]]
SEXP rcpp_bc_cplx_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
Rcomplex *pout;
pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

MACRO_OP_CPLX_MATH(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_cplx_ov, rng = false)]]
SEXP rcpp_bc_cplx_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
Rcomplex *pout;
pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);


MACRO_OP_CPLX_MATH(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_cplx_bv, rng = false)]]
SEXP rcpp_bc_cplx_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {

SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
Rcomplex *pout;
pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);


MACRO_OP_CPLX_MATH(MACRO_DIM_BIG2VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_cplx_d, rng = false)]]
SEXP rcpp_bc_cplx_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {

SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
Rcomplex *pout;
pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);


MACRO_OP_CPLX_MATH(
  MACRO_DIM_DOCALL
);

UNPROTECT(1);
return out;

}


