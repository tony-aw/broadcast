# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# Inlines ====
#


inlines <- "

inline SEXP inline_string_plus(
    String x, String y
  ) {
    if(x == NA_STRING || y == NA_STRING) {
      return(NA_STRING);
    }
    else {
      String out = x;
      out += y;
      return(out.get_sexp());
    }
}

"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#

macro_op_str_plus <- "
#define MACRO_OP_STR_PLUS(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      SET_STRING_ELT(out, flatind_out, inline_string_plus(tempx, tempy)),  \\
      const String tempx = px[flatind_x], \\
      const String tempy = py[flatind_y]  \\
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


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_str_plus, collapse = "\n"))


################################################################################
# Functions ====
#

prep <- "
const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
"

txt1 <- generate_functions(
  "bc_str", "StringVector", prep, "MACRO_OP_STR_PLUS"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macro_op_str_plus,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macro_op_str_plus,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_str.cpp")

