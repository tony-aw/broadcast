

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline int rcpp_cplx_equal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r == y.r && x.i == y.i) {
    return(1);
  }
  else return(0);
}


inline int rcpp_cplx_unequal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(R_isnancpp(x.r) || R_isnancpp(x.i) || R_isnancpp(y.r) || R_isnancpp(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r != y.r || x.i != y.i) {
    return(1);
  }
  else return(0);
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_cplx_v, rng = false)]]
SEXP rcpp_bcRel_cplx_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {




const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_CPLX_REL(MACRO_DIM_VECTOR);


UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_cplx_ov, rng = false)]]
SEXP rcpp_bcRel_cplx_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {




const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_CPLX_REL(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_cplx_bv, rng = false)]]
SEXP rcpp_bcRel_cplx_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {




const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_CPLX_REL(MACRO_DIM_BIG2VECTOR);

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_cplx_d, rng = false)]]
SEXP rcpp_bcRel_cplx_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {





const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_CPLX_REL(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


