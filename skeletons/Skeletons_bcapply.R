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


txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_v, rng = false)]]
void rcpp_bcapply_v(
  SEXP out, SEXP x, SEXP y, 
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_VECTOR);

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_ov, rng = false)]]
void rcpp_bcapply_ov(
  SEXP out, SEXP x, SEXP y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_ORTHOVECTOR);

}


"



txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_bv, rng = false)]]
void rcpp_bcapply_bv(
  SEXP out, SEXP x, SEXP y,  bool bigx, SEXP out_dim,
  R_xlen_t nout, Function f
) {

MACRO_OP_BCAPPLY(MACRO_DIM_BIG2VECTOR);

}


"




txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_d, rng = false)]]
void rcpp_bcapply_d(
  SEXP out, SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, Function f
) {


MACRO_OP_BCAPPLY(MACRO_DIM_DOCALL);


}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")

txt <- stringi::stri_c(
  header_for_package,
  txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

readr::write_file(txt, "src/rcpp_bcapply.cpp")

