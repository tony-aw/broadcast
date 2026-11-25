

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline int inline_boolint_mod(
  int x, int y
) {
  
  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  return inline_mod_body(x, y);
}


inline int inline_boolint_idiv(
  int x, int y, double intmin, double intmax
) {
  
  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  return inline_idiv_body(x, y);

}

inline int inline_boolint_gcd(int x, int y) {

  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) {
    return NA_INTEGER;
  }
  if(x == 0 && y == 0) return NA_INTEGER;
  
  return inline_gcd_body(x, y);
}





#define MACRO_ACTION_BOOLINT(DOCODE) do {  \
  if(inline_branchless_or(tempx == NA_INTEGER, tempy == NA_INTEGER)) {  \
    pout[flatind_out] = NA_INTEGER; \
  } \
  else {  \
    const long long tempout = DOCODE; \
    if(tempout > intmax || tempout < intmin) {  \
      warn = 1; \
    } \
    pout[flatind_out] = tempout;  \
  } \
} while(0)


#define MACRO_OP_BOOLINT(DIMCODE) do {	\
  bool warn = 0;                    \
  double intmax = DEF_INT32MAX;           \
  double intmin = -1 * intmax;          \
  switch(op) {	\
    case 1:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = inline_boolint_gcd(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );  \
      break;	\
    }	\
    case 2:	\
      {	\
        DIMCODE(  \
        pout[flatind_out] = inline_boolint_mod(tempx, tempy), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );  \
      break;	\
    }	\
    case 3:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = inline_boolint_idiv(tempx, tempy, intmin, intmax), \
        const int tempx = px[flatind_x],                                \
        const int tempy = py[flatind_y]                                 \
      );  \
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  }	\
  \
  if(warn) {                                        \
    warning("NAs produced by integer overflow");  \
  }                                                 \
} while(0)

  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_boolint_2v, rng = false)]]
  SEXP rcpp_bc_boolint_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  IntegerVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_boolint_1v, rng = false)]]
  SEXP rcpp_bc_boolint_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  IntegerVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_boolint_d, rng = false)]]
  SEXP rcpp_bc_boolint_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  IntegerVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);

  
  MACRO_OP_BOOLINT(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  