# set-up ====

library(stringi)
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "

inline double inline_int53_guard(
  double out, double int53min, double int53max
) {
  if(out > int53max) {
    return R_PosInf;
  }
  if(out < int53min) {
    return R_NegInf;
  }
  return trunc(out);
}


inline int inline_numtol_eq(
  double x, double y, double tol
) {
  const double calc = std::abs(x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc <= tol;
}


inline int inline_numtol_uneq(
  double x, double y, double tol
) {
  const double calc = std::abs(x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc > tol; 
}


inline int inline_numtol_less(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc < -tol;
}


inline int inline_numtol_greater(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc > tol;
}


inline int inline_numtol_leq(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc <= tol;
}


inline int inline_numtol_geq(
  double x, double y, double tol
) {
  const double calc = (x - y);
  return std::isnan(calc) ? NA_LOGICAL : calc >= -tol;
}




"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))



################################################################################
# MACROS ====
#

macro_typeswitch_tolerance <- "
#define MACRO_TYPESWITCH_TOLERANCE(DIMCODE, DOCODE) do {      \\
  if(use_int53guard) {                                        \\
    const double int53max = DEF_INT53MAX;                   \\
    const double int53min = -1 * int53max;                  \\
    DIMCODE(                                                \\
      pout[flatind_out] = DOCODE,                         \\
      const double tempx = inline_int53_guard(            \\
        px[flatind_x], int53min, int53max                 \\
      ),                                                  \\
      const double tempy = inline_int53_guard(            \\
        py[flatind_y], int53min, int53max                 \\
      )                                                   \\
    );                                                    \\
  }                                                       \\
  else {                                                  \\
    DIMCODE(                                              \\
      pout[flatind_out] = DOCODE,                         \\
      const double tempx =  px[flatind_x],                \\
      const double tempy =  py[flatind_y]                 \\
    );                                                    \\
  }                                                       \\
} while(0)


"



macro_op_tolerance <- "
#define MACRO_OP_TOLERANCE(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_eq(tempx, tempy, tol)                  \\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_uneq(tempx, tempy, tol)                  \\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_less(tempx, tempy, tol)                  \\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_greater(tempx, tempy, tol)                  \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_leq(tempx, tempy, tol)                  \\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_TYPESWITCH_TOLERANCE(         \\
        DIMCODE,                        \\
        inline_numtol_geq(tempx, tempy, tol)                  \\
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

macros_tolerance <- paste0(macro_typeswitch_tolerance, macro_op_tolerance)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_tolerance))




################################################################################
# Functions ====
#

prep <- "
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);
"

txt1 <- generate_functions(
  "bc_tolerance", "LogicalVector", prep, "MACRO_OP_TOLERANCE",
  additional_args = ", double tol, bool use_int53guard"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_tolerance,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_tolerance,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_tolerance.cpp")

