

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline double inline_int53_guard(
  double out, double int53min, double int53max
) {
  if(out > int53max) {
    return R_PosInf;
  }
  if(out < int53min) {
    return R_NegInf;
  }
  return trunc(out);
}


inline int inline_numtol_eq(
  double x, double y, double tol
) {
  const double calc = std::abs(x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc <= tol;
}


inline int inline_numtol_uneq(
  double x, double y, double tol
) {
  const double calc = std::abs(x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc > tol; 
}


inline int inline_numtol_less(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc < -tol;
}


inline int inline_numtol_greater(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc > tol;
}


inline int inline_numtol_leq(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc <= tol;
}


inline int inline_numtol_geq(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc >= -tol;
}





#define MACRO_TYPESWITCH_TOLERANCE(DIMCODE, DOCODE) do {      \
  if(use_int53guard) {                                        \
    const double int53max = DEF_INT53MAX;                   \
    const double int53min = -1 * int53max;                  \
    DIMCODE(                                                \
      pout[flatind_out] = DOCODE,                         \
      const double tempx = inline_int53_guard(            \
        px[flatind_x], int53min, int53max                 \
      ),                                                  \
      const double tempy = inline_int53_guard(            \
        py[flatind_y], int53min, int53max                 \
      )                                                   \
    );                                                    \
  }                                                       \
  else {                                                  \
    DIMCODE(                                              \
      pout[flatind_out] = DOCODE,                         \
      const double tempx =  px[flatind_x],                \
      const double tempy =  py[flatind_y]                 \
    );                                                    \
  }                                                       \
} while(0)



#define MACRO_OP_TOLERANCE(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_eq(tempx, tempy, tol)                  \
      );	\
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_uneq(tempx, tempy, tol)                  \
      );	\
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_less(tempx, tempy, tol)                  \
      );	\
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_greater(tempx, tempy, tol)                  \
      );	\
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_leq(tempx, tempy, tol)                  \
      );	\
      break;	\
    }	\
    case 6:	\
    {	\
      MACRO_TYPESWITCH_TOLERANCE(         \
        DIMCODE,                        \
        inline_numtol_geq(tempx, tempy, tol)                  \
      );	\
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
  // [[Rcpp::export(.rcpp_bc_tolerance_2v, rng = false)]]
  SEXP rcpp_bc_tolerance_2v(
    SEXP x, SEXP y, List prep, int op , double tol, bool use_int53guard
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);

  
  MACRO_OP_TOLERANCE(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_tolerance_1v, rng = false)]]
  SEXP rcpp_bc_tolerance_1v(
    SEXP x, SEXP y, List prep, int op , double tol, bool use_int53guard
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);

  
  MACRO_OP_TOLERANCE(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_tolerance_d, rng = false)]]
  SEXP rcpp_bc_tolerance_d(
    SEXP x, SEXP y, List prep, int op , double tol, bool use_int53guard
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);

  
  MACRO_OP_TOLERANCE(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  