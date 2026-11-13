# set-up ====
library(stringi)
source("SOURCE.R")


################################################################################
# MACROs ====
#


macro_acion_bool <- "
#define MACRO_ACTION_BOOL_RAW(DIMCODE, DOCODE) do {     \\
  DIMCODE(                                              \\
    pout[flatind_out] = DOCODE,                         \\
    const Rbyte tempx = px[flatind_x],                  \\
    const Rbyte tempy = py[flatind_y]                   \\
  );                                                    \\
} while(0)


"


macro_op_bool_raw <- "
#define MACRO_OP_BOOL_RAW(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, tempx && tempy);  \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, tempx || tempy);  \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx != (bool)tempy);  \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, !(tempx && tempy));  \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, !(tempx || tempy));  \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx == (bool)tempy);  \\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      MACRO_ACTION_BOOL_RAW(DIMCODE, (bool)tempx != (bool)tempy);  \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"

macros_bool <- paste0(macro_acion_bool,macro_op_bool_raw, collapse = "\n")


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_bool, collapse = "\n"))


################################################################################
# Functions ====
#

prep <- "
const Rbyte *px = RAW_RO(x);
const Rbyte *py = RAW_RO(y);
Rbyte *pout = RAW(out);
"

txt1 <- generate_functions("bc_b_raw", "RawVector", prep, "MACRO_OP_BOOL_RAW")
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_bool,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_bool,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_b_raw.cpp")

