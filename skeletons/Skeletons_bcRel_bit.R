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


txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_v, rng = false)]]
SEXP rcpp_bcRel_bit_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {

  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_VECTORSPECIAL);
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_VECTORSPECIAL);
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop(\"unsupported combinations of types given\");
  }

}


"


txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_d, rng = false)]]
SEXP rcpp_bcRel_bit_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_DOCALL);
    
    UNPROTECT(1);
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_DOCALL);
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop(\"unsupported combinations of types given\");
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
readr::write_file(txt, "src/rcpp_bcRel_bit.cpp")

