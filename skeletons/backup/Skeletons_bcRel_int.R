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

"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROS ====
#

macro_action_int_rel <- "
#define MACRO_ACTION_INT_REL(DIMCODE, DOCODE) do {        \\
  const double int53max = DEF_INT53MAX;                   \\
  const double int53min = -1 * int53max;                  \\
  DIMCODE(                                                \\
      pout[flatind_out] = DOCODE,                                                 \\
      const double tempx = inline_int53_guard(px[flatind_x], int53min, int53max), \\
      const double tempy = inline_int53_guard(py[flatind_y], int53min, int53max)  \\
    );                                                    \\
} while(0)

"

macro_op_int_rel <- "
#define MACRO_OP_INT_REL(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_eq(tempx, tempy, 0)                  \\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_uneq(tempx, tempy, 0)                  \\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_less(tempx, tempy, 0)                  \\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_greater(tempx, tempy, 0)                  \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_leq(tempx, tempy, 0)                  \\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_ACTION_INT_REL(         \\
        DIMCODE,                        \\
        inline_numdist_geq(tempx, tempy, 0)                  \\
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

macros_int_rel <- paste0(macro_action_int_rel, macro_op_int_rel)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_int_rel))





################################################################################
# Functions ====
#



prep <- "
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);
"

txt1 <- generate_functions(
  "bcRel_int", "LogicalVector", prep, "MACRO_OP_INT_REL"
)
cat(txt1)




txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_int_rel,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_int_rel,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_int.cpp")

