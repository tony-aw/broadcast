

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_cplx_v, rng = false)]]
SEXP rcpp_bcRel_cplx_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {




const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_CPLX_REL(MACRO_DIM_VECTORSPECIAL);


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


