# set-up ====
library(stringi)
source("SOURCE.r")

################################################################################
# MACROS ====
#

macro_action_dec_releq <- "
#define MACRO_ACTION_DEC_RELEQ(NACODE, DOCODE) do {   \\
  pout[flatind_out] = NACODE ? NA_LOGICAL : DOCODE; \\
} while(0)

"

macro_typeswitch_dec_releq <- "


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
      MACRO_ACTION_DEC_RELEQ(                               \\
        inline_branchless_or(TOKEN_NA_##TYPE_X(x), TOKEN_NA_##TYPE_Y(y)),   \\
        DOCODE                                                \\
      ),                                                      \\
      TOKEN_STORE_##TYPE_X(x),              \\
      TOKEN_STORE_##TYPE_Y(y)               \\
    );                                                        \\
} while(0)


#define MACRO_TYPESWITCH_DEC_RELEQ(DIMCODE, DOCODE) do {      \\
    bool isInt_x = TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP;  \\
    bool isInt_y = TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP;  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {          \\
      TOKEN_TYPESWITCH(double, double, DIMCODE, DOCODE);        \\
    }                                                           \\
    else if(isInt_x && TYPEOF(y) == REALSXP) {                  \\
      TOKEN_TYPESWITCH(int, double, DIMCODE, DOCODE);           \\
    }                                                           \\
    else if(TYPEOF(x) == REALSXP && isInt_y) {                  \\
      TOKEN_TYPESWITCH(double, int, DIMCODE, DOCODE);           \\
    }                                                           \\
    else if(isInt_x && isInt_y) {                               \\
      stop(\"use boolint_rel\");                                \\
    }                                                           \\
    else {                                                      \\
      stop(\"unsupported types given\");                        \\
    }                                                           \\
} while(0)

"



macro_op_dec_releq <- "
#define MACRO_OP_DEC_RELEQ(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_DEC_RELEQ(         \\
      DIMCODE,                        \\
      tempx == tempy                  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_DEC_RELEQ(         \\
      DIMCODE,                        \\
      tempx != tempy                  \\
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

macros_dec_releq <- paste0(macro_action_dec_releq, macro_typeswitch_dec_releq, macro_op_dec_releq)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_releq))



################################################################################
# Functions ====
#

txt1 <- generate_functions(
  "bcRelEq_dec", "LogicalVector", "int *pout = LOGICAL(out);", "MACRO_OP_DEC_RELEQ"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_dec_releq,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_dec_releq,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRelEq_dec.cpp")

