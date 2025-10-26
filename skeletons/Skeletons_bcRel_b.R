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
// [[Rcpp::export(.rcpp_bcRel_b_v, rng = false)]]
SEXP rcpp_bcRel_b_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_VECTOR);
   UNPROTECT(1);
   return out;
}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_VECTOR);
   UNPROTECT(1);
   return out;
}


}
"


txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_ov, rng = false)]]
SEXP rcpp_bcRel_b_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_ORTHOVECTOR);
   UNPROTECT(1);
   return out;

}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_ORTHOVECTOR);
   UNPROTECT(1);
   return out;
}

}
"



txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_bv, rng = false)]]
SEXP rcpp_bcRel_b_bv(
  SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
   const Rbyte *px = RAW_RO(x);
   const Rbyte *py = RAW_RO(y);
   SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
   Rbyte *pout;
   pout = RAW(out);
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_BIG2VECTOR);
   UNPROTECT(1);
   return out;

}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_BIG2VECTOR);
   UNPROTECT(1);
   return out;
}

}
"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_b_d, rng = false)]]
SEXP rcpp_bcRel_b_d(
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
   MACRO_OP_BOOL_REL_RAW(MACRO_DIM_DOCALL);
   UNPROTECT(1);
   return out;
}
else {
   const int *px = INTEGER_RO(x);
   const int *py = INTEGER_RO(y);
   SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
   int *pout;
   pout = LOGICAL(out); 
   MACRO_OP_BOOL_REL_INT(MACRO_DIM_DOCALL);
   UNPROTECT(1);
   return out;
}


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
readr::write_file(txt, "src/rcpp_bcRel_b.cpp")

