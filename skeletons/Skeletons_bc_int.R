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


inline bool rcpp_int53_need_guard1(
  SEXP x, SEXP y
) {
  if(TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
    if(TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP) {
      return false;
    }
  }
  return true;
}


inline bool rcpp_int53_need_guard2(
  SEXP x, SEXP y
) {
  if(TYPEOF(x) == INTSXP && TYPEOF(y) == LGLSXP) {
    return false;
  }
  if(TYPEOF(x) == LGLSXP && TYPEOF(y) == INTSXP) {
    return false;
  }
  return true;
}


inline double rcpp_int53_guard(
  double out, double intmin, double intmax
) {
  if(out > intmax) {
    return R_PosInf;
  }
  if(out < intmin) {
    return R_NegInf;
  }
  return out;
}


"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_v, rng = false)]]
SEXP rcpp_bc_int_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_VECTORSPECIAL);

UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_int_d, rng = false)]]
SEXP rcpp_bc_int_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

MACRO_OP_INT_MATH(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt0, txt1, txt2, 
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1, txt2, 
  collapse = "\n\n"
)

setwd("..")
readr::write_file(txt, "src/rcpp_bc_int.cpp")

