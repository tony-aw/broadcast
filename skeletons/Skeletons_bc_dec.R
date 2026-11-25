# set-up ====

library(stringi)
source("SOURCE.R")



################################################################################
# Inlines ====
#


inlines <- "

// I've noticed trying to out-smart the compiler,
// to make the code faster,
// is actually slowing down the integer arithmetic.
// So I'm just gonna do this the old straight-forward way...

inline int inline_int32_plus(
  int x, int y, long long intmax
) {

  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_INTEGER;
  }
  
  long long out = (long long)x + (long long)y;
  
  if(out > intmax || out < -intmax) {
    stop(\"error produced by integer overflow\");
  }
  
  return (int)out;
  
}


inline int inline_int32_min(
  int x, int y, long long intmax
) {
  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_INTEGER;
  }
  
  long long out = (long long)x - (long long)y;
  
  if(out > intmax || out < -intmax) {
    stop(\"error produced by integer overflow\");
  }
  
  return (int) out;
  
}

inline double inline_int32_mult(
  int x, int y
) {
  double out = inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ?
    NA_REAL :
    (double)x * (double)y;
  return out;
}

inline double inline_int32_div(
  int x, int y
) {
  double out = inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ?
    NA_REAL :
    (double)x / (double)y;
  return out;
}

inline double inline_int32_pow(
  int x, int y
) {

  if(x == 1 || y == 0) return 1;
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_REAL;
  return R_pow((double)x, (double)y);
}


inline double inline_dbl_pow(
  double x, double y
) {
  
  double out = inline_branchless_or(x == 1, y == 0) ? 1 : R_pow(x, y);
  return out;
}

"


################################################################################
# MACROS ====
#


macro_typeswitch_dec_math <- "



#define MACRO_TYPESWITCH_DEC_PLUSMIN(DIMCODE, REALCODE, INTEGERCODE) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      NumericVector out(no_init(nout)); \\
      double *pout = REAL(out);         \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = REALCODE,     \\
        const double tempx = px[flatind_x],    \\
        const double tempy = py[flatind_y]     \\
      );                                                \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                   \\
    else if(isInt_x && isInt_y) {       \\
      const long long intmax = (long long)DEF_INT32MAX;  \\
      IntegerVector out(no_init(nout)); \\
      int *pout = INTEGER(out);         \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = INTEGERCODE,     \\
        const int tempx = px[flatind_x],    \\
        const int tempy = py[flatind_y]     \\
      );                                                \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)



#define MACRO_TYPESWITCH_DEC_MDP(DIMCODE, REALCODE, INTEGERCODE) do { \\
    NumericVector out(no_init(nout)); \\
    double *pout = REAL(out);         \\
    \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = REALCODE,     \\
        const double tempx = px[flatind_x],    \\
        const double tempy = py[flatind_y]     \\
      );                                                \\
    }                                   \\
    else if(isInt_x && isInt_y) {       \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = INTEGERCODE,     \\
        const int tempx = px[flatind_x],    \\
        const int tempy = py[flatind_y]     \\
      );                                                \\
    }                                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
    out.attr(\"dim\") = out_dimorig;  \\
    return out;                       \\
} while(0)

"


macro_op_dec_math <- "
#define MACRO_OP_DEC_MATH(DIMCODE) do { \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_PLUSMIN(    \\
        DIMCODE,	        \\
        tempx + tempy,    \\
        inline_int32_plus(tempx, tempy, intmax)    \\
      );	                              \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_PLUSMIN(    \\
        DIMCODE,	        \\
        tempx - tempy,    \\
        inline_int32_min(tempx, tempy, intmax)    \\
      );	                              \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MDP(    \\
        DIMCODE,	        \\
        tempx * tempy,    \\
        inline_int32_mult(tempx, tempy)    \\
      );	                              \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MDP(    \\
        DIMCODE,	        \\
        tempx / tempy,    \\
        inline_int32_div(tempx, tempy)    \\
      );	                              \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MDP(    \\
        DIMCODE,	        \\
        inline_dbl_pow(tempx, tempy),    \\
        inline_int32_pow(tempx, tempy)    \\
      );	                              \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macros_dec_math <- paste0(macro_typeswitch_dec_math, macro_op_dec_math)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_math))


################################################################################
# Functions ====
#
# 
# 
# txt1 <- generate_functions(
#   "bc_dec", "NumericVector", "double *pout = REAL(out);", "MACRO_OP_DEC_MATH"
# )
# cat(txt1)


txt1 <- "

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
  
  
  MACRO_OP_DEC_MATH(MACRO_DIM_TWOVECTORS);
  
  return R_NilValue;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_dec_1v, rng = false)]]
  SEXP rcpp_bc_dec_1v(
    SEXP x, SEXP y, List prep, int op
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  MACRO_OP_DEC_MATH(MACRO_DIM_ONEVECTOR);
  
  return R_NilValue;
  
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

  MACRO_OP_DEC_MATH(MACRO_DIM_DOCALL);
  
  return R_NilValue;
  
  }



"
cat(txt1)

txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_dec_math,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "bc_dec.txt")

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_dec_math,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_dec.cpp")

