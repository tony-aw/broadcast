# set-up ====

library(stringi)

inlines <- readr::read_file("inlines.txt")
macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")
macro_op <- readr::read_file("macro_op.txt")

header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  inlines,
  "\n",
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
// [[Rcpp::export(.rcpp_bc_b_v, rng = false)]]
SEXP rcpp_bc_b_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {
  
  int tempout;
  
  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
     const Rbyte *px = RAW_RO(x);
     const Rbyte *py = RAW_RO(y);
     SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
     Rbyte *pout;
     pout = RAW(out);
     MACRO_OP_BOOL_ANDOR_RAW(MACRO_DIM_VECTORSPECIAL);
     UNPROTECT(1);
     return out;
  }
  else {
     const int *px = INTEGER_RO(x);
     const int *py = INTEGER_RO(y);
     SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
     int *pout;
     pout = LOGICAL(out); 
     MACRO_OP_BOOL_ANDOR_INT(MACRO_DIM_VECTORSPECIAL);
     UNPROTECT(1);
     return out;
  }


}
"


txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_d, rng = false)]]
SEXP rcpp_bc_b_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {

  
  int tempout;
  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
     const Rbyte *px = RAW_RO(x);
     const Rbyte *py = RAW_RO(y);
     SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
     Rbyte *pout;
     pout = RAW(out);
     MACRO_OP_BOOL_ANDOR_RAW(MACRO_DIM_DOCALL);
     UNPROTECT(1);
     return out;
  }
  else {
     const int *px = INTEGER_RO(x);
     const int *py = INTEGER_RO(y);
     SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
     int *pout;
     pout = LOGICAL(out); 
     MACRO_OP_BOOL_ANDOR_INT(MACRO_DIM_DOCALL);
     UNPROTECT(1);
     return out;
  }


}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt1, txt2,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  txt1, txt2,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_b.cpp")

