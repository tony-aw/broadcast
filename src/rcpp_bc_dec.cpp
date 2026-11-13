

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




#define MACRO_ACTION_DEC_MATH1(NACHECK, NARESULT, DOCODE) do {   \
  if(NACHECK) {                                     \
    pout[flatind_out] = NARESULT;                   \
  }                                                 \
  else {                                            \
    pout[flatind_out] = DOCODE;                     \
  }                                                 \
} while(0)


#define MACRO_ACTION_DEC_MATH2(RULECHECK, RULERESULT, DOCODE) do {   \
  if(RULECHECK) {                                   \
    pout[flatind_out] = RULERESULT;                 \
  }                                                 \
  else {                                            \
    pout[flatind_out] = DOCODE;                     \
  }                                                 \
} while(0)


#define MACRO_ACTION_DEC_MATH3(RULECHECK, RULERESULT, NACHECK, NARESULT, DOCODE) do {   \
  if(RULECHECK) {                                   \
    pout[flatind_out] = RULERESULT;                 \
  }                                                 \
  else if(NACHECK) {                                \
    pout[flatind_out] = NARESULT;                   \
  }                                                 \
  else {                                            \
    pout[flatind_out] = DOCODE;                     \
  }                                                 \
} while(0)



#define MACRO_TYPESWITCH_DEC_MATH1(DIMCODE, DECIMALCODE, INTEGERCODE, NARESULT) do { \
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \
                                                                  \
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \
      const double *px = REAL_RO(x);    \
      const double *py = REAL_RO(y);    \
      DIMCODE(                          \
        pout[flatind_out] = DECIMALCODE,    \
        const double tempx = px[flatind_x], \
        const double tempy = py[flatind_y]  \
      );                                \
    }                                   \
    else if(isInt_x && isInt_y) {                                 \
      const int *px = INTEGER_RO(x);    \
      const int *py = INTEGER_RO(y);    \
      DIMCODE(                          \
        MACRO_ACTION_DEC_MATH1(                        \
          tempx == NA_INTEGER || tempy == NA_INTEGER,   \
          NARESULT,                                     \
          INTEGERCODE                                   \
        ),                                              \
        const double tempx = (double) px[flatind_x],    \
        const double tempy = (double) py[flatind_y]     \
      );                                                \
    }                                                   \
    else {                                                        \
      stop("unsupported types given");        \
    }                                           \
} while(0)


#define MACRO_TYPESWITCH_DEC_MATH2(DIMCODE, RULECHECK, RULERESULT, DECIMALCODE, INTEGERCODE, NARESULT) do { \
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \
                                                                  \
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \
      const double *px = REAL_RO(x);    \
      const double *py = REAL_RO(y);    \
      DIMCODE(                          \
        MACRO_ACTION_DEC_MATH2(                        \
          RULECHECK,                                    \
          RULERESULT,                                   \
          DECIMALCODE                                   \
        ),                                              \
        const double tempx = px[flatind_x],             \
        const double tempy = py[flatind_y]              \
      );                                \
    }                                   \
    else if(isInt_x && isInt_y) {                                 \
      const int *px = INTEGER_RO(x);    \
      const int *py = INTEGER_RO(y);    \
      DIMCODE(                          \
        MACRO_ACTION_DEC_MATH3(                        \
          RULECHECK,                                    \
          RULERESULT,                                   \
          tempx == NA_INTEGER || tempy == NA_INTEGER,   \
          NARESULT,                                     \
          INTEGERCODE                                   \
        ),                                              \
        const double tempx = (double) px[flatind_x],    \
        const double tempy = (double) py[flatind_y]     \
      );                                                \
    }                                                   \
    else {                                              \
      stop("unsupported types given");        \
    }                                           \
} while(0)


#define MACRO_OP_DEC_MATH(DIMCODE) do { \
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_TYPESWITCH_DEC_MATH1(    \
        DIMCODE,	        \
        tempx + tempy,    \
        tempx + tempy,    \
        NA_REAL           \
      );	                              \
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_TYPESWITCH_DEC_MATH1(    \
        DIMCODE,	        \
        tempx - tempy,    \
        tempx - tempy,    \
        NA_REAL           \
      );	                              \
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_TYPESWITCH_DEC_MATH1(    \
        DIMCODE,	        \
        tempx * tempy,    \
        tempx * tempy,    \
        NA_REAL           \
      );	                              \
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_DEC_MATH1(    \
        DIMCODE,	        \
        tempx / tempy,    \
        tempx / tempy,    \
        NA_REAL           \
      );	                              \
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_DEC_MATH2(    \
        DIMCODE,	\
        tempx == 1 || tempy == 0,	\
        1,	\
        R_pow(tempx, tempy),    \
        R_pow(tempx, tempy),    \
        NA_REAL                 \
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
  // [[Rcpp::export(.rcpp_bc_dec_2v, rng = false)]]
  SEXP rcpp_bc_dec_2v(
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
  
  MACRO_OP_DEC_MATH(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_dec_d, rng = false)]]
  SEXP rcpp_bc_dec_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  NumericVector out(no_init(nout));
  
  double *pout = REAL(out);
  
  MACRO_OP_DEC_MATH(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  