# set-up ====

library(stringi)

macro_dim <- readr::read_file("macro_dim.txt")
macro_ternary <- readr::read_file("macro_ternary.txt")

header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  macro_dim,
  "\n",
  macro_ternary,
  "\n"
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
// [[Rcpp::export(.rcpp_bcTern_int_v, rng = false)]]
SEXP rcpp_bcTern_int_v(
  SEXP values, SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {

SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
const Rbyte truevalue = RAW_RO(values)[0];
const Rbyte falsevalue = RAW_RO(values)[1];
const Rbyte navalue = RAW_RO(values)[2];

MACRO_TERNARY_RELOP_INT(MACRO_DIM_VECTORSPECIAL);

UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcTern_int_d, rng = false)]]
SEXP rcpp_bcTern_int_d(
  SEXP values, SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));
Rbyte *pout = RAW(out);
const Rbyte truevalue = RAW_RO(values)[0];
const Rbyte falsevalue = RAW_RO(values)[1];
const Rbyte navalue = RAW_RO(values)[2];

MACRO_TERNARY_RELOP_INT(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;


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
readr::write_file(txt, "src/rcpp_bcTern_int.cpp")



# test ====


x <- array(c(1:9, NA), c(10, 1))
y <- array(c(NA, 9:1), c(1, 10))
out <- matrix(as.raw(100), 10, 10)
values <- as.raw(c(1, 0, 255))
x.dim <- dim(x)
y.dim <- dim(y)
out.dim <- dim(out)
out.len <- length(out)
out <- .rcpp_bcTern_int_v(values, x, y, x.dim, y.dim, out.dim, out.len, 2L, TRUE, 1)
dim(out) <- out.dim
out
broadcast::bc.rel(x, y, "==")
