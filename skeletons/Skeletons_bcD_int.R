# set-up ====

library(stringi)

macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")
macro_op <- readr::read_file("macro_op.txt")

header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  macro_action,
  "\n",
  macro_dim,
  "\n",
  macro_typeswitch_numeric,
  "\n",
  macro_op
)


header_for_package <- "

#include <Rcpp/Lightest>
#include \"broadcast.h\"

using namespace Rcpp;


"

readr::write_file(header_for_sourcing, "header.txt")

Rcpp::sourceCpp(code = header_for_sourcing)



################################################################################
# Functions ====
#


txt0 <- "


inline long long rcpp_int53_gcd_rec(
  long long x, long long y
) {

  long long a = labs(x);
  long long b = labs(y);
  
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
    return rcpp_int53_gcd_rec(a - b, b);
  }
  return rcpp_int53_gcd_rec(a, b - a);
}


inline double rcpp_int53_gcd(
  double x, double y
) {
  
  if(x == 0 && y == 0) {
    return NA_REAL;
  }
  else if(x == 0) {
    return y;
  }
  else if(y == 0) {
    return x;
  }
  
  return rcpp_int53_gcd_rec((long long) x, (long long) y);
}


inline double rcpp_int53_mod(double x, double y, double intmin, double intmax) {
  if(y == 0) {
    return R_NaN;
  }
  if(MACRO_OVERFLOW(x)) {
    return R_NaN;
  }
  if(MACRO_OVERFLOW(y) && std::abs(x) <= std::abs(y)) {
    double out1 = (std::abs(x) == std::abs(y)) ? 0 :
	    ((x < 0 && y > 0) ||
	     (y < 0 && x > 0))
	     ? x+y  // differing signs
	     : x;
	   return out1;
  }
  
  double q = x / y;
  double tmp = x - floor(q) * y;
  double out = (double) (tmp - floorl(tmp / y) * y);
  return out;
}


inline double rcpp_int53_idiv(double x, double y, double intmin, double intmax) {
    
    double q = x / y;
    
    if (y == 0.0 || MACRO_OVERFLOW(q) || !R_FINITE(q)) {
      return q;
    }
    
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


"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_int_v, rng = false)]]
SEXP rcpp_bcD_int_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_FACT(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_int_ov, rng = false)]]
SEXP rcpp_bcD_int_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_FACT(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}


"



txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_int_bv, rng = false)]]
SEXP rcpp_bcD_int_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_FACT(MACRO_DIM_BIG2VECTOR);

UNPROTECT(1);
return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_int_d, rng = false)]]
SEXP rcpp_bcD_int_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_FACT(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt0, txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

setwd("..")
readr::write_file(txt, "src/rcpp_bcD_int.cpp")

