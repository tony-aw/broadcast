

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




#define MACRO_OVERFLOW_INT53(REF) ((REF) < int53min || (REF) > int53max)

inline double inline_int53_overflow2NA(
  double x, double int53min, double int53max
) {
  if(x < int53min) return NA_REAL;
  if(x > int53max) return NA_REAL;
  return trunc(x);
}

inline double inline_int53_overflow2Inf(
  double x, double int53min, double int53max
) {
  if(x < int53min) return R_NegInf;
  if(x > int53max) return R_PosInf;
  return trunc(x);
}


inline double inline_int53_mod(double x, double y, double int53min, double int53max) {
  if(y == 0) {
    return R_NaN;
  }
  if(MACRO_OVERFLOW_INT53(x)) {
    return R_NaN;
  }
  if(MACRO_OVERFLOW_INT53(y) && std::abs(x) <= std::abs(y)) {
    double out1 = (std::abs(x) == std::abs(y)) ? 0 :
	    ((x < 0 && y > 0) ||
	     (y < 0 && x > 0))
	     ? x+y  // differing signs
	     : x;
	   return out1;
  }
  
  return inline_mod_body(x, y);
}


inline double inline_int53_idiv(double x, double y, double int53min, double int53max) {
    
    double q = x / y;
    
    if (y == 0.0 || MACRO_OVERFLOW_INT53(q) || !R_FINITE(q)) {
      return q;
    }
    
    return inline_idiv_body(x, y, q);
  }


double inline_int53_gcd(
  long long x, long long y
) {
  
  if(x == 0 && y == 0) {
    return NA_REAL;
  }
  
  return inline_gcd_body(x, y);
}






#define MACRO_ACTION_INT(NACHECK, DOCODE) do {     \
  if(NACHECK) {                                         \
    pout[flatind_out] = NA_REAL;                        \
  }                                                     \
  else {                                                \
    pout[flatind_out] = DOCODE;                         \
  }                                                     \
} while(0)





#define TOKEN_DECLARE_double(var) const double *p##var = REAL_RO(var)
#define TOKEN_DECLARE_int(var) const int *p##var = INTEGER_RO(var)
#define TOKEN_NA_double(var) std::isnan(temp##var)
#define TOKEN_NA_int(var) temp##var == NA_INTEGER
#define TOKEN_STORE_double(var, OVERFLOWFUN)  const double temp##var = OVERFLOWFUN(p##var[flatind_##var], int53min, int53max)
#define TOKEN_STORE_int(var, OVERFLOWFUN)  const double temp##var = (double)p##var[flatind_##var]

#define TOKEN_TYPESWITCH(TYPE_X, TYPE_Y, DIMCODE, DOCODE, OVERFLOWFUN) do { \
  TOKEN_DECLARE_##TYPE_X(x);  \
  TOKEN_DECLARE_##TYPE_Y(y);  \
  DIMCODE(                                                  \
      MACRO_ACTION_INT(                               \
        TOKEN_NA_##TYPE_X(x) || TOKEN_NA_##TYPE_Y(y),   \
        DOCODE                                                \
      ),                                                      \
      TOKEN_STORE_##TYPE_X(x, OVERFLOWFUN),              \
      TOKEN_STORE_##TYPE_Y(y, OVERFLOWFUN)               \
    );                                                        \
} while(0)

#define MACRO_TYPESWITCH_INT(DIMCODE, DOCODE, OVERFLOWFUN) do {      \
  bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \
  bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \
  double int53max = DEF_INT53MAX;                                 \
  double int53min = -1 * int53max;                                \
  if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {  \
      TOKEN_TYPESWITCH(double, double, DIMCODE, DOCODE, OVERFLOWFUN);  \
  }                                                   \
  else if(isInt_x && isInt_y) {        \
     stop("use boolmath");           \
  }                                                         \
  else {                                                    \
    stop("unsupported types given");                      \
  }                                                         \
} while(0)


#define MACRO_OP_INT(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_TYPESWITCH_INT(  \
        DIMCODE,	\
        (double)inline_int53_gcd(tempx, tempy), \
        inline_int53_overflow2NA  \
      );    \
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_TYPESWITCH_INT(  \
          DIMCODE,	\
          inline_int53_mod(tempx, tempy, int53min, int53max), \
          inline_int53_overflow2Inf \
        );	\
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_TYPESWITCH_INT(  \
          DIMCODE,	\
         inline_int53_idiv(tempx, tempy, int53min, int53max), \
         inline_int53_overflow2Inf  \
        );	\
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_INT( \
          DIMCODE,	\
         tempx < tempy ? tempx : tempy, \
         inline_int53_overflow2Inf  \
        );	\
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_INT( \
          DIMCODE,	\
         tempx > tempy ? tempx : tempy, \
         inline_int53_overflow2Inf  \
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
  // [[Rcpp::export(.rcpp_bc_int_2v, rng = false)]]
  SEXP rcpp_bc_int_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  NumericVector out(no_init(nout));
  
  
double *pout = REAL(out);

  
  MACRO_OP_INT(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_int_d, rng = false)]]
  SEXP rcpp_bc_int_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  NumericVector out(no_init(nout));
  
  
double *pout = REAL(out);

  
  MACRO_OP_INT(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  