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

Rcpp::sourceCpp(code = header_for_sourcing)



################################################################################
# Functions ====
#


txt0 <- "

inline Rbyte rcpp_raw_diff(Rbyte x, Rbyte y) {
  Rbyte out = (x > y) ? (x - y) : (y - x);
  return out;
}


"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_v, rng = false)]]
SEXP rcpp_bc_raw_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_VECTOR);

UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_ov, rng = false)]]
SEXP rcpp_bc_raw_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}


"


txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_bv, rng = false)]]
SEXP rcpp_bc_raw_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_BIG2VECTOR);

UNPROTECT(1);
return out;

}


"



txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_raw_d, rng = false)]]
SEXP rcpp_bc_raw_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);

MACRO_OP_RAW_BYTE(MACRO_DIM_DOCALL);

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
readr::write_file(txt, "src/rcpp_bc_raw.cpp")

