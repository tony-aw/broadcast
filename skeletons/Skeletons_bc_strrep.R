# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# Inlines ====
#


inlines <- "

inline SEXP inline_string_mult(
    String x, int y
  ) {
    if(x == NA_STRING || y < 0) {
      return(NA_STRING);
    }
    else if(y == 0) {
      String out = \"\";
      return out.get_sexp();
    }
    else if(y == 1) {
      return x.get_sexp();
    }
    else {
      String out = x;
      for(int i = 1; i < y; ++i) {
        out += x;
      }
      return out.get_sexp();
    }
}

"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROS ====
#

macro_strrep <- "
#define MACRO_STRREP(DIMCODE) do { \\
  DIMCODE(  \\
    SET_STRING_ELT(out, flatind_out, inline_string_mult(tempx, tempy)), \\
    const String tempx = px[flatind_x], \\
    const int tempy = py[flatind_y] \\
  );  \\
} while(0)


"


################################################################################
# Functions ====
#


prep <- "
const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);
"

txt1 <- generate_functions(
  "bc_strrep", "StringVector", prep, "MACRO_STRREP"
)
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macro_strrep,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)



setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macro_strrep,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_strrep.cpp")

