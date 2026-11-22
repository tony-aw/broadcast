# set-up ====
library(stringi)
source("SOURCE.R")

################################################################################
# Inlines ====
#

inlines <- "

#define MACRO_OVERFLOW_INT53(REF) ((REF) < int53min || (REF) > int53max)

inline double inline_int53_overflow2NA(
  double x, double int53min, double int53max
) {
  if(x < int53min) return NA_REAL;
  if(x > int53max) return NA_REAL;
  return inline_int53_round(x, int53min, int53max);
}


inline double inline_int53_mod(double x, double y) {

  // Note:
  // input will already be 53bit integer;
  // std::isfinite() thus checks for both overflow (= Inf or -Inf) AND NaNs simultaneously

  if(y == 0) {
    return R_NaN;
  }
  if(!std::isfinite(x)) {
    return R_NaN;
  }
  if(!std::isfinite(y) && std::abs(x) <= std::abs(y)) {
    double out1 = (std::abs(x) == std::abs(y)) ? 0 :
	    ((x < 0 && y > 0) ||
	     (y < 0 && x > 0))
	     ? x+y  // differing signs
	     : x;
	   return out1;
  }
  
  return inline_mod_body(x, y);
}


inline double inline_int53_idiv(double x, double y) {
  
  // Note:
  // input will already be 53bit integer;
  // std::isfinite() thus checks for both overflow (= Inf or -Inf) AND NaNs simultaneously
  // std::isinf(), however, checks for overflow/Inf only.
  
  // Handle Infinities and NaNs:
  if(std::isinf(y)) {
    if(std::isinf(x)) return R_NaN;
    if(x == 0.0) return x / y;
    return (std::signbit(x) != std::signbit(y)) ? -1.0 : 0.0;
  }
  
  // Handle weird double cases:
  if(!std::isfinite(x) || !std::isfinite(y) || y == 0.0) {
    return std::floor(x / y);
  }
  
  // preserve signed zero, just in case;
  if( x == 0.0)  return x / y;
  
  // In all other cases, go to main long long function:
  return inline_idiv_body(x, y);
  
}



double inline_int53_gcd(
  long long x, long long y
) {
  
  if(x == 0 && y == 0) {
    return NA_REAL;
  }
  
  return inline_gcd_body(x, y);
}



"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#



macro_action_int <- "


#define MACRO_ACTION_INT(NACHECK, DOCODE) do {     \\
  if(NACHECK) {                                         \\
    pout[flatind_out] = NA_REAL;                        \\
  }                                                     \\
  else {                                                \\
    pout[flatind_out] = DOCODE;                         \\
  }                                                     \\
} while(0)


"

macro_typeswitch_int <- "


#define TOKEN_DECLARE_double(var) const double *p##var = REAL_RO(var)
#define TOKEN_DECLARE_int(var) const int *p##var = INTEGER_RO(var)
#define TOKEN_NA_double(var) std::isnan(temp##var)
#define TOKEN_NA_int(var) temp##var == NA_INTEGER
#define TOKEN_STORE_double(var, OVERFLOWFUN)  const double temp##var = OVERFLOWFUN(p##var[flatind_##var], int53min, int53max)
#define TOKEN_STORE_int(var, OVERFLOWFUN)  const double temp##var = (double)p##var[flatind_##var]

#define TOKEN_TYPESWITCH(TYPE_X, TYPE_Y, DIMCODE, DOCODE, OVERFLOWFUN) do { \\
  TOKEN_DECLARE_##TYPE_X(x);  \\
  TOKEN_DECLARE_##TYPE_Y(y);  \\
  DIMCODE(                                                  \\
      MACRO_ACTION_INT(                               \\
        TOKEN_NA_##TYPE_X(x) || TOKEN_NA_##TYPE_Y(y),   \\
        DOCODE                                                \\
      ),                                                      \\
      TOKEN_STORE_##TYPE_X(x, OVERFLOWFUN),              \\
      TOKEN_STORE_##TYPE_Y(y, OVERFLOWFUN)               \\
    );                                                        \\
} while(0)

#define MACRO_TYPESWITCH_INT(DIMCODE, DOCODE, OVERFLOWFUN) do {      \\
  bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
  bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
  double int53max = DEF_INT53MAX;                                 \\
  double int53min = -1 * int53max;                                \\
  if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {  \\
      TOKEN_TYPESWITCH(double, double, DIMCODE, DOCODE, OVERFLOWFUN);  \\
  }                                                   \\
  else if(isInt_x && isInt_y) {        \\
     stop(\"use boolmath\");           \\
  }                                                         \\
  else {                                                    \\
    stop(\"unsupported types given\");                      \\
  }                                                         \\
} while(0)

"


macro_op_int <- "
#define MACRO_OP_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_INT(  \\
        DIMCODE,	\\
        (double)inline_int53_gcd(tempx, tempy), \\
        inline_int53_overflow2NA  \\
      );    \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_INT(  \\
          DIMCODE,	\\
          inline_int53_mod(tempx, tempy), \\
          inline_int53_round \\
        );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_INT(  \\
          DIMCODE,	\\
         inline_int53_idiv(tempx, tempy), \\
         inline_int53_round  \\
        );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_INT( \\
          DIMCODE,	\\
         tempx < tempy ? tempx : tempy, \\
         inline_int53_round  \\
        );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_INT( \\
          DIMCODE,	\\
         tempx > tempy ? tempx : tempy, \\
         inline_int53_round  \\
        );	\\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"

macros_int <- paste0(macro_action_int, macro_typeswitch_int, macro_op_int, collapse = "\n")

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, macros_int, collapse = "\n"))


################################################################################
# Functions ====
#



prep <- "
double *pout = REAL(out);
"

txt1 <- generate_functions(
  "bc_int", "NumericVector", prep, "MACRO_OP_INT"
)
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macros_int,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macros_int,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_int.cpp")

