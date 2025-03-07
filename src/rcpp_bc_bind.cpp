#include <Rcpp.h>
#include "Broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_bind)]]
void rcpp_bc_bind(
  SEXP out, SEXP x,
  SEXP starts, SEXP ends, SEXP by_x,
  SEXP dcp_out, SEXP dcp_x, SEXP out_dim
) {


  switch(TYPEOF(out)) {
    case RAWSXP:
    {
      Rbyte *pout = RAW(out);
      Rbyte *px = RAW(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case LGLSXP:
    {
      int *pout = LOGICAL(out);
      int *px = LOGICAL(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case INTSXP:
    {
      int *pout = INTEGER(out);
      int *px = INTEGER(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case REALSXP:
    {
      double *pout = REAL(out);
      double *px = REAL(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case CPLXSXP:
    {
      Rcomplex *pout = COMPLEX(out);
      Rcomplex *px = COMPLEX(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case STRSXP:
    {
      SEXP *pout = STRING_PTR(out);
      SEXP *px = STRING_PTR(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case VECSXP:
    {
      MACRO_DIM_BIND_DOCALL(SET_VECTOR_ELT(out, flatind_out, VECTOR_ELT(x, flatind_x)));
      break;
    }
  }

}