# set-up ====
library(stringi)
source("SOURCE.R")



################################################################################
# MACROS ====
#

macro_action_dec_pminmax <- "

#define MACRO_ACTION_DEC_PMINMAX(NACHECK, DOCODE) do {   \\
  pout[flatind_out] = NACHECK ? NA_REAL : DOCODE; \\
} while(0)


"

macro_typeswitch_dec_pminmax <- "


#define TOKEN_DECLARE_double(var) const double *p##var = REAL_RO(var)
#define TOKEN_DECLARE_int(var) const int *p##var = INTEGER_RO(var)
#define TOKEN_NA_double(var) std::isnan(temp##var)
#define TOKEN_NA_int(var) temp##var == NA_INTEGER
#define TOKEN_STORE_double(var)  const double temp##var = p##var[flatind_##var]
#define TOKEN_STORE_int(var)  const int temp##var = p##var[flatind_##var]

#define TOKEN_TYPESWITCH(TYPE_X, TYPE_Y, DIMCODE, DOCODE) do { \\
  TOKEN_DECLARE_##TYPE_X(x);  \\
  TOKEN_DECLARE_##TYPE_Y(y);  \\
  DIMCODE(                                                  \\
      MACRO_ACTION_DEC_PMINMAX(                               \\
        TOKEN_NA_##TYPE_X(x) || TOKEN_NA_##TYPE_Y(y),   \\
        DOCODE                                                \\
      ),                                                      \\
      TOKEN_STORE_##TYPE_X(x),              \\
      TOKEN_STORE_##TYPE_Y(y)               \\
    );                                                        \\
} while(0)


#define MACRO_TYPESWITCH_DEC_PMINMAX(DIMCODE, DOCODE) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {  \\
      TOKEN_TYPESWITCH(double, double, DIMCODE, DOCODE);                 \\
    }                                                   \\
    else if(isInt_x && isInt_y) {       \\
      stop(\"use boolmath\");           \\
    }                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)

"




macro_op_dec_pminmax<- "
#define MACRO_OP_DEC_PMINMAX(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_PMINMAX(	        \\
        DIMCODE,                            \\
        (tempx < tempy) ? tempx : tempy     \\
      );	                                  \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_PMINMAX(	        \\
        DIMCODE,                            \\
        (tempx > tempy) ? tempx : tempy     \\
      );	                                  \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macros_dec_pminmax <- paste0(
  macro_action_dec_pminmax, macro_typeswitch_dec_pminmax, macro_op_dec_pminmax
)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_pminmax))


################################################################################
# Functions ====
#

txt1 <- generate_functions(
  "bcP_dec", "NumericVector", "double *pout = REAL(out);", "MACRO_OP_DEC_PMINMAX"
)
cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  macros_dec_pminmax,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_dec_pminmax,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcP_dec.cpp")

