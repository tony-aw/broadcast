# set-up ====
library(stringi)
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "

inline int inline_boolint_mod(
  int x, int y
) {
  
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  return inline_mod_body(x, y);
}


inline int inline_boolint_idiv(
  int x, int y, double intmin, double intmax
) {
  
  if(x == NA_INTEGER || y == NA_INTEGER || y == 0) return NA_INTEGER;

  double q = x / y;
  
  if (y == 0.0 || q < intmin || q  > intmax || !R_FINITE(q)) return NA_INTEGER;
  
  return inline_idiv_body(x, y, q);

}

inline int inline_boolint_gcd(int x, int y) {

  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_INTEGER;
  }
  if(x == 0 && y == 0) return NA_INTEGER;
  
  return inline_gcd_body(x, y);
}


inline int inline_boolint_pmin(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  return x < y ? x : y;
}

inline int inline_boolint_pmax(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  return x > y ? x : y;
}



"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# Macro ====
#


macro_op_boolint <- "
#define MACRO_OP_BOOLINT(DIMCODE) do {	\\
  double intmax = pow(2, 31) - 1;           \\
  double intmin = -1 * intmax;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_pmin(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_pmax(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_gcd(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 4:	\\
      {	\\
        DIMCODE(  \\
        pout[flatind_out] = inline_boolint_mod(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_idiv(tempx, tempy, intmin, intmax), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_boolint, collapse = "\n"))



################################################################################
# Functions ====
#


prep <- "
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);
"

txt1 <- generate_functions(
  "bc_boolint", "IntegerVector", prep, "MACRO_OP_BOOLINT"
)

cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macro_op_boolint,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_op_boolint,
  txt1, 
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_boolint.cpp")

