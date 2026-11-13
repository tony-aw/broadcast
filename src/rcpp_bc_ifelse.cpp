
#include <Rcpp/Lightest>
#include "broadcast.h"
using namespace Rcpp;

static inline Rcomplex inline_cplx_returnNA() {
    Rcomplex out;
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }


#define MACRO_SET_C(VAR, INDEX, VALUE) do {   \
  VAR[INDEX] = VALUE;                         \
} while(0)


#define MACRO_ACTION_IFELSE1(SETFUN, VAR) do {                               \
  SETFUN(VAR, flatind_out, (bool)pcond[flatind_out] ? tempx : tempy);   \
} while(0)                                                              \


#define MACRO_ACTION_IFELSE2(SETFUN, VAR, NACODE) do {                       \
  if(pcond[flatind_out] == NA_INTEGER) {                                \
    SETFUN(VAR, flatind_out, NACODE);                                   \
  }                                                                     \
  else {                                                                \
   SETFUN(VAR, flatind_out, (bool)pcond[flatind_out] ? tempx : tempy);  \
  }                                                                     \
} while(0)                                                              \


#define MACRO_ACTION_IFELSE3(NACHECK, NACODE, DOCODE) do {        \
  if(NACHECK) {                                                   \
    NACODE;                                                       \
  }                                                               \
  else {                                                          \
   DOCODE;                                                        \
  }                                                               \
} while(0)                                                        \





#define MACRO_OP_IFELSE_INT(DIMCODE) do {       \
  switch(TYPEOF(x)) {	\
    case LGLSXP:	\
    case INTSXP:	\
    {	\
      const int *px = INTEGER_RO(x);	\
      const int *py = INTEGER_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(TYPEOF(x), nout));	\
      int *pout;	\
      pout = INTEGER(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE2(	            \
          MACRO_SET_C,                 \
          pout,                         \
          NA_INTEGER                      \
        ),                                \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );      \
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case REALSXP:	\
    {	\
      const double *px = REAL_RO(x);	\
      const double *py = REAL_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));	\
      double *pout;	\
      pout = REAL(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE2(	              \
          MACRO_SET_C,                      \
          pout,                             \
          NA_REAL                           \
        ),                                  \
        const double tempx = px[flatind_x], \
        const double tempy = py[flatind_y]  \
      );      \
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case CPLXSXP:	\
    {	\
      const Rcomplex *px = COMPLEX_RO(x);	\
      const Rcomplex *py = COMPLEX_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));	\
      Rcomplex *pout;	\
      pout = COMPLEX(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE2(	                \
          MACRO_SET_C,                        \
          pout,                               \
          inline_cplx_returnNA()              \
        ),                                    \
        const Rcomplex tempx = px[flatind_x], \
        const Rcomplex tempy = py[flatind_y]  \
      );      \
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *px = STRING_PTR_RO(x);	\
      const SEXP *py = STRING_PTR_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(STRSXP, nout));	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE2(	            \
          SET_STRING_ELT,                 \
          out,                            \
          NA_STRING                       \
        ),                                \
        const SEXP tempx = px[flatind_x], \
        const SEXP tempy = py[flatind_y]  \
      );      \
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case RAWSXP:	\
    {	\
      const Rbyte *px = RAW_RO(x);	\
      const Rbyte *py = RAW_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));	\
      Rbyte *pout = RAW(out); \
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE3(	                \
          pcond[flatind_out] == NA_INTEGER,                       \
          stop("NA condition not supported for type of `raw`"), \
          pout[flatind_out] = pcond[flatind_out] ? tempx : tempy	\
        ),                                    \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );      \
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case VECSXP:	\
    {	\
      SEXP out = PROTECT(Rf_allocVector(VECSXP, nout));	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE2(	            \
          SET_VECTOR_ELT,                 \
          out,                            \
          R_NilValue                      \
        ),                                \
        const SEXP tempx = VECTOR_ELT(x, flatind_x),  \
        const SEXP tempy = VECTOR_ELT(y, flatind_y)   \
      );      \
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
} while(0)



#define MACRO_OP_IFELSE_RAW(DIMCODE) do {       \
  switch(TYPEOF(x)) {	\
    case LGLSXP:	\
    case INTSXP:	\
    {	\
      const int *px = INTEGER_RO(x);	\
      const int *py = INTEGER_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));	\
      int *pout;	\
      pout = INTEGER(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(MACRO_SET_C, pout),   \
        const int tempx = px[flatind_x],        \
        const int tempy = py[flatind_y]         \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case REALSXP:	\
    {	\
      const double *px = REAL_RO(x);	\
      const double *py = REAL_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));	\
      double *pout;	\
      pout = REAL(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(MACRO_SET_C, pout),   \
        const double tempx = px[flatind_x],        \
        const double tempy = py[flatind_y]         \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case CPLXSXP:	\
    {	\
      const Rcomplex *px = COMPLEX_RO(x);	\
      const Rcomplex *py = COMPLEX_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));	\
      Rcomplex *pout;	\
      pout = COMPLEX(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(MACRO_SET_C, pout),   \
        const Rcomplex tempx = px[flatind_x],        \
        const Rcomplex tempy = py[flatind_y]         \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *px = STRING_PTR_RO(x);	\
      const SEXP *py = STRING_PTR_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(STRSXP, nout));	\
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(SET_STRING_ELT, out),   \
        const SEXP tempx = px[flatind_x],       \
        const SEXP tempy = py[flatind_y]        \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case RAWSXP:	\
    {	\
      const Rbyte *px = RAW_RO(x);	\
      const Rbyte *py = RAW_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));	\
      Rbyte *pout = RAW(out); \
      	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(MACRO_SET_C, pout),   \
        const Rbyte tempx = px[flatind_x],        \
        const Rbyte tempy = py[flatind_y]         \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case VECSXP:	\
    {	\
      SEXP out = PROTECT(Rf_allocVector(VECSXP, nout));	\
      DIMCODE(	\
        MACRO_ACTION_IFELSE1(SET_VECTOR_ELT, out),   \
        const SEXP tempx = VECTOR_ELT(x, flatind_x),        \
        const SEXP tempy = VECTOR_ELT(y, flatind_y)         \
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
} while(0)



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_2v, rng = false)]]
SEXP rcpp_bc_ifelse_2v(
  SEXP cond, SEXP x, SEXP y, List prep
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_TWOVECTORS
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_TWOVECTORS
    );
  }
  else {
    stop("unsupported type given");
  }


}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_1v, rng = false)]]
SEXP rcpp_bc_ifelse_1v(
  SEXP cond, SEXP x, SEXP y, List prep
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_ONEVECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_ONEVECTOR
    );
  }
  else {
    stop("unsupported type given");
  }


}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_d, rng = false)]]
SEXP rcpp_bc_ifelse_d(
  SEXP cond, SEXP x, SEXP y, List prep
) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_DOCALL
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_DOCALL
    );
  }
  else {
    stop("unsupported type given");
  }

}


