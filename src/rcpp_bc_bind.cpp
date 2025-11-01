

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_bind_prep)]]
void rcpp_bc_bind_prep(
  SEXP starts, SEXP ends, SEXP by_x, SEXP dcp_x, SEXP dim_x, SEXP dim_out, int along, int size_along, int counter, int n
) {

  // get properties:
  const int *pdim_out = INTEGER_RO(dim_out);
  const int *pdim_x = INTEGER_RO(dim_x);
  const int ndim_x = Rf_length(dim_x);
  
  
  // starts:
  int *pstart = INTEGER(starts);
  for(int i = 0; i < n; ++i) {
    pstart[i] = 0;
  }
  pstart[along] = counter;
  
  
  // ends:
  int *pend = INTEGER(ends);
  for (int i = 0; i < n; ++i) {
    pend[i] = pdim_out[i] - 1;
  }
  pend[along] = counter + size_along - 1;
  
  
  // by_x:
  int *pby_x = INTEGER(by_x);
  for(int i = 0; i < ndim_x; ++i) {
    pby_x[i] = pdim_x[i] > 1 ? 1 : 0;
  }
  if(n > ndim_x) {
    for(int i = ndim_x; i < n; ++i) {
      pby_x[i] = 0;
    }
  }
  pby_x[along] = 1;
  
  
  // dcp_x:
  double *pdcp_x = REAL(dcp_x);
  double temp_prod = pdim_x[0];
  pdcp_x[0] = 1;
  pdcp_x[1] = pdim_x[0];
  if((ndim_x+1) > 2) {
    for(int i = 2; i < (ndim_x+1); ++i) {
      temp_prod = temp_prod * pdim_x[i-1];
      pdcp_x[i] = temp_prod;
    }
  }
  if((n+1) > (ndim_x+1)) {
    for(int i = (ndim_x+1); i < (n+1); ++i) {
      pdcp_x[i] = temp_prod;
    }
  }
  
  
}

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
      const SEXP *px = STRING_PTR_RO(x);
      MACRO_DIM_BIND_DOCALL(SET_STRING_ELT(out, flatind_out, px[flatind_x]));
      break;
    }
    case VECSXP:
    {
      MACRO_DIM_BIND_DOCALL(SET_VECTOR_ELT(out, flatind_out, VECTOR_ELT(x, flatind_x)));
      break;
    }
  }

}


