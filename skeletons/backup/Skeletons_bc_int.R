# set-up ====

source("SOURCE.R")

################################################################################
# Inlines ====
#

inlines <- "

inline double inline_coerce_int2dbl(int x) {
  if(x == NA_INTEGER) return NA_REAL;
  return (double)x;
}

bool rcpp_int53_need_guard(
  SEXP x, SEXP y
) {
  if(TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
    if(TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP) {
      return false;
    }
  }
  return true;
}


inline double inline_int53_guard(
  double out, double int53min, double int53max
) {
  if(out > int53max) {
    return R_PosInf;
  }
  if(out < int53min) {
    return R_NegInf;
  }
  return out;
}



"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))




################################################################################
# MACROs ====
#

macro_typeswitch_int_math <- "


#define MACRO_TYPESWITCH_INT_UNGUARDED(DIMCODE, DOCODE) do {      \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      pout[flatind_out] = DOCODE,                                     \\
      const double tempx = inline_coerce_int2dbl(px[flatind_x]), \\
      const double tempy = inline_coerce_int2dbl(py[flatind_y])    \\
    );                                                       \\
} while(0)


#define MACRO_TYPESWITCH_INT_GUARDED(DIMCODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {   \\
    const double *px = REAL_RO(x);                          \\
    const double *py = REAL_RO(y);                          \\
    DIMCODE(                                                  \\
      pout[flatind_out] = inline_int53_guard(DOCODE, int53min, int53max), \\
      const double tempx = trunc(px[flatind_x]),              \\
      const double tempy = trunc(py[flatind_y])               \\
    );                                                        \\
  }                                                           \\
  else if(xint && yint) {                                         \\
    const int *px = INTEGER_RO(x);                                \\
    const int *py = INTEGER_RO(y);                                \\
    DIMCODE(                                                      \\
      pout[flatind_out] = inline_int53_guard(DOCODE, int53min, int53max), \\
      const double tempx = inline_coerce_int2dbl(px[flatind_x]),    \\
      const double tempy = inline_coerce_int2dbl(py[flatind_y])     \\
    );                                                      \\
  }                                                         \\
} while(0)



#define MACRO_GUARDSWITCH_INT(DIMCODE, GUARDCHECK, DOCODE) do {      \\
  if(!GUARDCHECK) {    \\
    MACRO_TYPESWITCH_INT_UNGUARDED(	\\
      DIMCODE,	\\
      DOCODE	\\
    );	\\
  }   \\
  else {    \\
    MACRO_TYPESWITCH_INT_GUARDED(	\\
      DIMCODE,	\\
      DOCODE    \\
    );	\\
  }   \\
} while(0)




"



macro_op_int_math <- "
#define MACRO_OP_INT_MATH(DIMCODE) do {	\\
  double int53max = DEF_INT53MAX;           \\
  double int53min = -1 * int53max;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_GUARDSWITCH_INT(            \\
        DIMCODE,                        \\
        rcpp_int53_need_guard(x, y),   \\
        tempx + tempy                   \\
      );                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_GUARDSWITCH_INT(            \\
        DIMCODE,                        \\
        rcpp_int53_need_guard(x, y),   \\
        tempx - tempy                   \\
      );                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_INT_GUARDED(            \\
        DIMCODE,                        \\
        tempx * tempy                   \\
      );                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"



macros_int_math <- paste0(
  macro_typeswitch_int_math, macro_op_int_math, collapse = "\n"
)

Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_int_math, collapse = "\n"))


################################################################################
# Functions ====
#




prep <- "
double *pout = REAL(out);
"

txt1 <- generate_functions(
  "bc_int", "NumericVector", prep, "MACRO_OP_INT_MATH"
)
cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_int_math,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_int_math,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_int.cpp")

