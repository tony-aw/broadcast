

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline int inline_boolint_eq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x == y;
}


inline int inline_boolint_uneq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x != y;
}


inline int inline_boolint_lesser(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x < y;
}


inline int inline_boolint_greater(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x > y;
}


inline int inline_boolint_leq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x <= y;
}


inline int inline_boolint_geq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x >= y;
}


#define MACRO_OP_BOOLINT_REL(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    case 7: \
    {	\
      DIMCODE( \
        pout[flatind_out] = inline_boolint_eq(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );  \
      break;	\
    }	\
    case 2:	\
    case 8: \
    {	\
      DIMCODE( \
        pout[flatind_out] = inline_boolint_uneq(tempx, tempy),  \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      ); \
      break;	\
    }	\
    case 3:	\
    case 9: \
    {	\
      DIMCODE( \
        pout[flatind_out] = inline_boolint_lesser(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );  \
      break;	\
    }	\
    case 4:	\
    case 10:\
    {	\
      DIMCODE(  \
        pout[flatind_out] = inline_boolint_greater(tempx, tempy),  \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );	\
      break;	\
    }	\
    case 5:	\
    case 11:  \
    {	\
      DIMCODE(  \
        pout[flatind_out] = inline_boolint_leq(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );	\
      break;	\
    }	\
    case 6:	\
    case 12:  \
    {	\
      DIMCODE(  \
        pout[flatind_out] = inline_boolint_geq(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
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
  // [[Rcpp::export(.rcpp_bcRel_boolint_2v, rng = false)]]
  SEXP rcpp_bcRel_boolint_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT_REL(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_boolint_1v, rng = false)]]
  SEXP rcpp_bcRel_boolint_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT_REL(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRel_boolint_d, rng = false)]]
  SEXP rcpp_bcRel_boolint_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT_REL(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  