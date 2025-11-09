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
inline Rcomplex rcpp_cplx_returnNA() {
    Rcomplex out;
    out.r = NA_REAL;
    out.i = NA_REAL;
    return out;
  }
"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_v, rng = false)]]
SEXP rcpp_bc_ifelse_v(
  SEXP cond, SEXP x, SEXP y,
  R_xlen_t nout
) {


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_VECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_VECTOR
    );
  }
  else {
    stop(\"unsupported type given\");
  }


}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_ov, rng = false)]]
SEXP rcpp_bc_ifelse_ov(
  SEXP cond, SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout
) {


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_ORTHOVECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_ORTHOVECTOR
    );
  }
  else {
    stop(\"unsupported type given\");
  }

}


"




txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_bv, rng = false)]]
SEXP rcpp_bc_ifelse_bv(
  SEXP cond, SEXP x, SEXP y, bool bigx, SEXP out_dim,
  R_xlen_t nout
) {


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_BIG2VECTOR
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_BIG2VECTOR
    );
  }
  else {
    stop(\"unsupported type given\");
  }

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_ifelse_d, rng = false)]]
SEXP rcpp_bc_ifelse_d(
  SEXP cond, SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout
) {


  if(TYPEOF(cond) == LGLSXP || TYPEOF(cond) == INTSXP) {
    const int *pcond = INTEGER_RO(cond);
    
    MACRO_OP_IFELSE_INT(
      MACRO_DIM_DOCALL
    );
  }
  else if(TYPEOF(cond) == RAWSXP) {
    const Rbyte *pcond = RAW_RO(cond);
    
    MACRO_OP_IFELSE_RAW(
      MACRO_DIM_DOCALL
    );
  }
  else {
    stop(\"unsupported type given\");
  }

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt0, txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)



setwd("..")

txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_ifelse.cpp")
