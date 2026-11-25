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
  
  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  return inline_mod_body(x, y);
}


inline int inline_boolint_idiv(
  int x, int y, double intmin, double intmax
) {
  
  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  return inline_idiv_body(x, y);

}

inline int inline_boolint_gcd(int x, int y) {

  if(inline_branchless_or(x == NA_INTEGER, y == NA_INTEGER)) {
    return NA_INTEGER;
  }
  if(x == 0 && y == 0) return NA_INTEGER;
  
  return inline_gcd_body(x, y);
}



"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# Macro ====
#

macro_action_boolint <- "

#define MACRO_ACTION_BOOLINT(DOCODE) do {  \\
  if(inline_branchless_or(tempx == NA_INTEGER, tempy == NA_INTEGER)) {  \\
    pout[flatind_out] = NA_INTEGER; \\
  } \\
  else {  \\
    const long long tempout = DOCODE; \\
    if(tempout > intmax || tempout < intmin) {  \\
      warn = 1; \\
    } \\
    pout[flatind_out] = tempout;  \\
  } \\
} while(0)

"

macro_op_boolint <- "
#define MACRO_OP_BOOLINT(DIMCODE) do {	\\
  bool warn = 0;                    \\
  double intmax = DEF_INT32MAX;           \\
  double intmin = -1 * intmax;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_gcd(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 2:	\\
      {	\\
        DIMCODE(  \\
        pout[flatind_out] = inline_boolint_mod(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 3:	\\
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
  \\
  if(warn) {                                        \\
    warning(\"NAs produced by integer overflow\");  \\
  }                                                 \\
} while(0)
"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_action_boolint, macro_op_boolint, collapse = "\n"))



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
  inlines, macro_action_boolint, macro_op_boolint,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_action_boolint, macro_op_boolint,
  txt1, 
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_boolint.cpp")

