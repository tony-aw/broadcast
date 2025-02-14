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
  macro_op,
  "\n"
)


header_for_package <- "

#include <Rcpp/Lightest>
#include \"Broadcast.h\"

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
// [[Rcpp::export(.rcpp_bcRel_str_v)]]
SEXP rcpp_bcRel_str_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_VECTOR);


UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_ov)]]
SEXP rcpp_bcRel_str_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {


int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}


"

txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_bs)]]
SEXP rcpp_bcRel_str_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_BIGSMALL_DOCALL);

UNPROTECT(1);
return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_str_d)]]
SEXP rcpp_bcRel_str_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



int tempout;

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_STR_REL(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

txt <- stringi::stri_c(
  header_for_package,
  txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_str.cpp")

