# set-up ====
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "

inline int inline_boolint_eq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x == y;
}


inline int inline_boolint_uneq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x != y;
}


inline int inline_boolint_lesser(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x < y;
}


inline int inline_boolint_greater(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x > y;
}


inline int inline_boolint_leq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x <= y;
}


inline int inline_boolint_geq(
  int x, int y
) {
  return inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER) ? NA_INTEGER : x >= y;
}

"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# Macro ====
#


macro_op_boolint_rel <- "
#define MACRO_OP_BOOLINT_REL(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    case 7: \\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_eq(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 2:	\\
    case 8: \\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_uneq(tempx, tempy),  \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      ); \\
      break;	\\
    }	\\
    case 3:	\\
    case 9: \\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_lesser(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 4:	\\
    case 10:\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_greater(tempx, tempy),  \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    case 11:  \\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_leq(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    case 12:  \\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_geq(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
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


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_boolint_rel, collapse = "\n"))


################################################################################
# Functions ====
#


prep <- "
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);
"

txt1 <- generate_functions(
  "bcRel_boolint", "LogicalVector", prep, "MACRO_OP_BOOLINT_REL"
)

cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macro_op_boolint_rel,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_op_boolint_rel,
  txt1, 
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_boolint.cpp")

