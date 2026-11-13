# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# MACROS ====
#

macro_action_dec_math <- "

#define MACRO_ACTION_DEC_MATH1(NACHECK, NARESULT, DOCODE) do {   \\
  if(NACHECK) {                                     \\
    pout[flatind_out] = NARESULT;                   \\
  }                                                 \\
  else {                                            \\
    pout[flatind_out] = DOCODE;                     \\
  }                                                 \\
} while(0)


#define MACRO_ACTION_DEC_MATH2(RULECHECK, RULERESULT, DOCODE) do {   \\
  if(RULECHECK) {                                   \\
    pout[flatind_out] = RULERESULT;                 \\
  }                                                 \\
  else {                                            \\
    pout[flatind_out] = DOCODE;                     \\
  }                                                 \\
} while(0)


#define MACRO_ACTION_DEC_MATH3(RULECHECK, RULERESULT, NACHECK, NARESULT, DOCODE) do {   \\
  if(RULECHECK) {                                   \\
    pout[flatind_out] = RULERESULT;                 \\
  }                                                 \\
  else if(NACHECK) {                                \\
    pout[flatind_out] = NARESULT;                   \\
  }                                                 \\
  else {                                            \\
    pout[flatind_out] = DOCODE;                     \\
  }                                                 \\
} while(0)

"

macro_typeswitch_dec_math <- "

#define MACRO_TYPESWITCH_DEC_MATH1(DIMCODE, DECIMALCODE, INTEGERCODE, NARESULT) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = DECIMALCODE,    \\
        const double tempx = px[flatind_x], \\
        const double tempy = py[flatind_y]  \\
      );                                \\
    }                                   \\
    else if(isInt_x && isInt_y) {                                 \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                          \\
        MACRO_ACTION_DEC_MATH1(                        \\
          tempx == NA_INTEGER || tempy == NA_INTEGER,   \\
          NARESULT,                                     \\
          INTEGERCODE                                   \\
        ),                                              \\
        const double tempx = (double) px[flatind_x],    \\
        const double tempy = (double) py[flatind_y]     \\
      );                                                \\
    }                                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)


#define MACRO_TYPESWITCH_DEC_MATH2(DIMCODE, RULECHECK, RULERESULT, DECIMALCODE, INTEGERCODE, NARESULT) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                          \\
        MACRO_ACTION_DEC_MATH2(                        \\
          RULECHECK,                                    \\
          RULERESULT,                                   \\
          DECIMALCODE                                   \\
        ),                                              \\
        const double tempx = px[flatind_x],             \\
        const double tempy = py[flatind_y]              \\
      );                                \\
    }                                   \\
    else if(isInt_x && isInt_y) {                                 \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                          \\
        MACRO_ACTION_DEC_MATH3(                        \\
          RULECHECK,                                    \\
          RULERESULT,                                   \\
          tempx == NA_INTEGER || tempy == NA_INTEGER,   \\
          NARESULT,                                     \\
          INTEGERCODE                                   \\
        ),                                              \\
        const double tempx = (double) px[flatind_x],    \\
        const double tempy = (double) py[flatind_y]     \\
      );                                                \\
    }                                                   \\
    else {                                              \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)

"




macro_op_dec_math <- "
#define MACRO_OP_DEC_MATH(DIMCODE) do { \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MATH1(    \\
        DIMCODE,	        \\
        tempx + tempy,    \\
        tempx + tempy,    \\
        NA_REAL           \\
      );	                              \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MATH1(    \\
        DIMCODE,	        \\
        tempx - tempy,    \\
        tempx - tempy,    \\
        NA_REAL           \\
      );	                              \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MATH1(    \\
        DIMCODE,	        \\
        tempx * tempy,    \\
        tempx * tempy,    \\
        NA_REAL           \\
      );	                              \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MATH1(    \\
        DIMCODE,	        \\
        tempx / tempy,    \\
        tempx / tempy,    \\
        NA_REAL           \\
      );	                              \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_DEC_MATH2(    \\
        DIMCODE,	\\
        tempx == 1 || tempy == 0,	\\
        1,	\\
        R_pow(tempx, tempy),    \\
        R_pow(tempx, tempy),    \\
        NA_REAL                 \\
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


macros_dec_math <- paste0(macro_action_dec_math, macro_typeswitch_dec_math, macro_op_dec_math)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_math))


################################################################################
# Functions ====
#


txt1 <- generate_functions(
  "bc_dec", "NumericVector", "double *pout = REAL(out);", "MACRO_OP_DEC_MATH"
)
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_dec_math,
  txt1,
  collapse = "\n\n"
)
Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_dec_math,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_dec.cpp")

