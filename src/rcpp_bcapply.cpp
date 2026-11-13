
#include <Rcpp/Lightest>
#include "broadcast.h"
using namespace Rcpp;

#define MACRO_OP_BCAPPLY(DIMCODE) do {       \
  switch(TYPEOF(out)) {	\
    case LGLSXP:	\
    {	\
      int *pout;	\
      pout = LOGICAL(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = LOGICAL_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \
      );	\
      break; \
    }	\
    case INTSXP:	\
    {	\
      int *pout;	\
      pout = INTEGER(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = INTEGER_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \
      );	\
      break; \
    }	\
    case REALSXP:	\
    {	\
      double *pout;	\
      pout = REAL(out);	\
      DIMCODE(	\
        pout[flatind_out] = REAL_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,   \
      );	\
      break; \
    }	\
    case CPLXSXP:	\
    {	\
      Rcomplex *pout;	\
      pout = COMPLEX(out);	\
      DIMCODE(	\
        pout[flatind_out] = COMPLEX_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \
      );	\
      break; \
    }	\
    case STRSXP:	\
    {	\
      	\
      DIMCODE(	\
        SET_STRING_ELT(out, flatind_out, STRING_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0)), ,   \
      );	\
      break; \
    }	\
    case RAWSXP:	\
    {	\
      Rbyte *pout;	\
      pout = RAW(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = RAW_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \
      );	\
      break; \
    }	\
    case VECSXP:	\
    {	\
     DIMCODE(	\
        SET_VECTOR_ELT(out, flatind_out, f(x, y, flatind_x + 1, flatind_y + 1)), ,    \
      );	\
      break; \
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
} while(0)

  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_2v, rng = false)]]
void rcpp_bcapply_2v(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_TWOVECTORS);

}



  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_1v, rng = false)]]
void rcpp_bcapply_1v(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_ONEVECTOR);

}



  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_d, rng = false)]]
void rcpp_bcapply_d(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_DOCALL);

}


