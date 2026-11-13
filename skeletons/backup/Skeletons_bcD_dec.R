# set-up ====

library(stringi)
source("SOURCE.R")


################################################################################
# MACROS ====
#

macro_action_dec_dist <- "
#define MACRO_ACTION_DEC_DIST(DIMCODE, DOCODE) do {       \\
  DIMCODE(                                                \\
      pout[flatind_out] = DOCODE,                         \\
      const double tempx = px[flatind_x],                 \\
      const double tempy = py[flatind_y]                  \\
    );                                                    \\
} while(0)


"



macro_op_dec_dist <- "
#define MACRO_OP_DEC_DIST(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_eq(tempx, tempy, prec)                  \\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_uneq(tempx, tempy, prec)                  \\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_less(tempx, tempy, prec)                  \\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_greater(tempx, tempy, prec)                  \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_leq(tempx, tempy, prec)                  \\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_ACTION_DEC_DIST(         \\
        DIMCODE,                        \\
        inline_numdist_geq(tempx, tempy, prec)                  \\
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

macros_dec_dist <- paste0(macro_action_dec_dist, macro_op_dec_dist)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_dist))




################################################################################
# Functions ====
#

prep <- "
int *pout = LOGICAL(out);
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);
"

txt1 <- generate_functions(
  "bcD_dec", "LogicalVector", prep, "MACRO_OP_DEC_DIST",
  additional_args = ", double prec"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_dec_dist,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_dec_dist,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcD_dec.cpp")

