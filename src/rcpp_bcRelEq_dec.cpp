

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;



#define MACRO_ACTION_DEC_RELEQ(NACODE, DOCODE) do {   \
  pout[flatind_out] = NACODE ? NA_LOGICAL : DOCODE; \
} while(0)




#define TOKEN_DECLARE_double(var) const double *p##var = REAL_RO(var)
#define TOKEN_DECLARE_int(var) const int *p##var = INTEGER_RO(var)
#define TOKEN_NA_double(var) std::isnan(temp##var)
#define TOKEN_NA_int(var) temp##var == NA_INTEGER
#define TOKEN_STORE_double(var)  const double temp##var = p##var[flatind_##var]
#define TOKEN_STORE_int(var)  const int temp##var = p##var[flatind_##var]

#define TOKEN_TYPESWITCH(TYPE_X, TYPE_Y, DIMCODE, DOCODE) do { \
  TOKEN_DECLARE_##TYPE_X(x);  \
  TOKEN_DECLARE_##TYPE_Y(y);  \
  DIMCODE(                                                  \
      MACRO_ACTION_DEC_RELEQ(                               \
        inline_branchless_or(TOKEN_NA_##TYPE_X(x), TOKEN_NA_##TYPE_Y(y)),   \
        DOCODE                                                \
      ),                                                      \
      TOKEN_STORE_##TYPE_X(x),              \
      TOKEN_STORE_##TYPE_Y(y)               \
    );                                                        \
} while(0)


#define MACRO_TYPESWITCH_DEC_RELEQ(DIMCODE, DOCODE) do {      \
    bool isInt_x = TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP;  \
    bool isInt_y = TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP;  \
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {          \
      TOKEN_TYPESWITCH(double, double, DIMCODE, DOCODE);        \
    }                                                           \
    else if(isInt_x && TYPEOF(y) == REALSXP) {                  \
      TOKEN_TYPESWITCH(int, double, DIMCODE, DOCODE);           \
    }                                                           \
    else if(TYPEOF(x) == REALSXP && isInt_y) {                  \
      TOKEN_TYPESWITCH(double, int, DIMCODE, DOCODE);           \
    }                                                           \
    else if(isInt_x && isInt_y) {                               \
      stop("use boolint_rel");                                \
    }                                                           \
    else {                                                      \
      stop("unsupported types given");                        \
    }                                                           \
} while(0)


#define MACRO_OP_DEC_RELEQ(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    MACRO_TYPESWITCH_DEC_RELEQ(         \
      DIMCODE,                        \
      tempx == tempy                  \
    );	\
    break;	\
  }	\
  case 2:	\
  {	\
    MACRO_TYPESWITCH_DEC_RELEQ(         \
      DIMCODE,                        \
      tempx != tempy                  \
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
  // [[Rcpp::export(.rcpp_bcRelEq_dec_2v, rng = false)]]
  SEXP rcpp_bcRelEq_dec_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  int *pout = LOGICAL(out);
  
  MACRO_OP_DEC_RELEQ(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRelEq_dec_1v, rng = false)]]
  SEXP rcpp_bcRelEq_dec_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  int *pout = LOGICAL(out);
  
  MACRO_OP_DEC_RELEQ(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcRelEq_dec_d, rng = false)]]
  SEXP rcpp_bcRelEq_dec_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  int *pout = LOGICAL(out);
  
  MACRO_OP_DEC_RELEQ(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  