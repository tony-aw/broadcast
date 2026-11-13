# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# Inlines ====
#

inlines <- "

inline int inline_str_eq(SEXP x, SEXP y) {
  if(x == NA_STRING || y == NA_STRING) {
    return NA_LOGICAL;
  }
  else {
    return (int)R_compute_identical(x, y, 0);
  }
}


inline int inline_str_uneq(SEXP x, SEXP y) {
  if(x == NA_STRING || y == NA_STRING) {
    return NA_LOGICAL;
  }
  else {
    return (int)!R_compute_identical(x, y, 0);
  }
}

"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#


macro_op_str_rel <- "
#define MACRO_OP_STR_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_str_eq(tempx, tempy),  \\
      const SEXP tempx = px[flatind_x], \\
      const SEXP tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_str_uneq(tempx, tempy),  \\
      const SEXP tempx = px[flatind_x], \\
      const SEXP tempy = py[flatind_y]  \\
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


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_str_rel, collapse = "\n"))


################################################################################
# Functions ====
#


prep <- "

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout = LOGICAL(out);

"

txt1 <- generate_functions(
  "bcRel_str", "LogicalVector", prep, "MACRO_OP_STR_REL"
)

cat(txt1)




txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macro_op_str_rel,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_op_str_rel,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_str.cpp")

