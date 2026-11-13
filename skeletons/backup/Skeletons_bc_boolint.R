# set-up ====
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "


inline int inline_boolint_plus(
  int x, int y, int intmax
) {
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(x == intmax || y == intmax) {
    if(x == 1 || y == 1) {
      return NA_INTEGER;
    }
  }
  return x + y;
}


inline int inline_boolint_min(
  int x, int y, int intmin
) {
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(x == intmin || y == intmin) {
    if(x == 1 || y == 1) {
      return NA_INTEGER;
    }
  }
  return x - y;
}


inline int inline_boolint_mult(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  return x * y;
}


inline int inline_boolint_pow(
  int x, int y
) {
  if(x == 1 || y == 0) return 1;
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(x == 0 && y == 1) return 0;
  return NA_INTEGER;
}



inline int inline_boolint_mod(
  int x, int y
) {
  
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(y == 0) return NA_INTEGER;
  
  double a = x;
  double b = y;
  double q = a / b;
  double tmp = a - floor(q) * b;
  int out = (double) (tmp - floorl(tmp / b) * b);
  return out;
}


inline int inline_boolint_idiv(
  int x, int y, double intmin, double intmax
) {
  
  if(x == NA_INTEGER || y == NA_INTEGER || y == 0) return NA_INTEGER;

  double q = x / y;
  
  if (y == 0.0 || q < intmin || q  > intmax || !R_FINITE(q)) return NA_INTEGER;
  
  if(std::abs(q) < 1) {
    double z = (q < 0) ? -1
    : ((x < 0 && y > 0) ||
       (x > 0 && y < 0) // differing signs
       ? -1 : 0);
    return z;
  }
  
  double tmp = (double)(x - floor(q) * (double)y);
  return (double) (floor(q) + floorl(tmp/y));

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


macro_op_boolint_math <- "
#define MACRO_OP_BOOLINT_MATH(DIMCODE) do {	\\
  double intmax = pow(2, 31) - 1;           \\
  double intmin = -1 * intmax;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_plus(tempx, tempy, intmax), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_min(tempx, tempy, intmin),  \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      ); \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE( \\
        pout[flatind_out] = inline_boolint_mult(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_pow(tempx, tempy),  \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_pmin(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = inline_boolint_pmax(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );	\\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = rcpp_boolint_gcd(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 8:	\\
      {	\\
        DIMCODE(  \\
        pout[flatind_out] = inline_boolint_mod(tempx, tempy), \\
        const int tempx = px[flatind_x],                                \\
        const int tempy = py[flatind_y]                                 \\
      );  \\
      break;	\\
    }	\\
    case 9:	\\
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


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_boolint_math, collapse = "\n"))


################################################################################
# Helper Functions ====
#

helpers <- "


int rcpp_boolint_gcd_rec(
  int x, int y
) {

  int a = std::abs(x);
  int b = std::abs(y);
  
  if (a == 0) {
    return b;
  }
  if (b == 0) {
    return a;
  }
  if (a == b) {
    return a;
  }
  if (a > b) {
    return rcpp_boolint_gcd_rec(a - b, b);
  }
  return rcpp_boolint_gcd_rec(a, b - a);
}

int rcpp_boolint_gcd(
  int x, int y
) {
  
  if(x == NA_INTEGER || y == NA_INTEGER) return NA_INTEGER;
  if(x == 0 && y == 0)  return NA_INTEGER;
  if(x == 0)  return y;
  if(y == 0)  return x;
  if(x == 1 || y == 1) return 1;
  
  return rcpp_boolint_gcd_rec(x, y);
}


"



################################################################################
# Functions ====
#


prep <- "
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = INTEGER(out);
"

txt1 <- generate_functions(
  "bc_boolint", "INTSXP", prep, "MACRO_OP_BOOLINT_MATH"
)

cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macro_op_boolint_math, helpers,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_op_boolint_math, helpers,
  txt1, 
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_boolint.cpp")

