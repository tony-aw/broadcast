# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# MACROs ====
#


macro_op_raw_rel <- "
#define MACRO_OP_RAW_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx == tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx != tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx < tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx > tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx <= tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = tempx >= tempy, \\
      const Rbyte tempx = px[flatind_x],  \\
      const Rbyte tempy = py[flatind_y] \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_raw_rel, collapse = "\n"))


################################################################################
# Functions ====
#

prep <- "
int *pout = LOGICAL(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);
"

txt1 <- generate_functions(
  "bcRel_raw", "LogicalVector", prep, "MACRO_OP_RAW_REL"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  macro_op_raw_rel,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macro_op_raw_rel,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_raw.cpp")

