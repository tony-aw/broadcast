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
// [[Rcpp::export(.rcpp_bc_ifelse_v)]]
SEXP rcpp_bc_ifelse_v(
  SEXP cond, SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

const int *pcond = INTEGER(cond);


switch(TYPEOF(x)) {
  case LGLSXP:
  {
    const int *px = INTEGER(x);
    const int *py = INTEGER(y);
    
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
    int *pout;
    pout = LOGICAL(out);
    
    MACRO_DIM_VECTOR(
      MACRO_ACTION2(
        pcond[flatind_out] == NA_LOGICAL,
        pout[flatind_out] = NA_LOGICAL,
        pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]
      )
    );
    
    UNPROTECT(1);
    return out;
    
  }
  default:
  {
    stop(\"unsupported type\");
  }
}

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_ov)]]
SEXP rcpp_bc_b_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}


"

txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_bs)]]
SEXP rcpp_bc_b_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_BIGSMALL_DOCALL);

UNPROTECT(1);
return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_b_d)]]
SEXP rcpp_bc_b_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


int tempout;

int xTRUE, xFALSE, xNA, yTRUE, yFALSE, yNA;

const int *px = INTEGER(x);
const int *py = INTEGER(y);

SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));
int *pout;
pout = LOGICAL(out);

MACRO_OP_B_ANDOR(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1, txt2, txt3, txt4,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_b.cpp")

