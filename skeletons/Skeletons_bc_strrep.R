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

inline String rcpp_string_mult(
    String x, int y
  ) {
    if(x == NA_STRING || y < 0) {
      return(NA_STRING);
    }
    else if(y == 0) {
      return \"\";
    }
    else if(y == 1) {
      return x;
    }
    else {
      String out = x;
      for(int i = 1; i < y; ++i) {
        out += x;
      }
      return(out);
    }
}

"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_v, rng = false)]]
SEXP rcpp_bc_strrep_v(
  SEXP x, SEXP y, 
  R_xlen_t nout
) {

const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);
MACRO_DIM_VECTOR(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));


return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_ov, rng = false)]]
SEXP rcpp_bc_strrep_ov(
  SEXP x, SEXP y,  bool RxC, SEXP out_dim,
  R_xlen_t nout
) {

const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);

MACRO_DIM_ORTHOVECTOR(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));


return out;

}


"


txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_bv, rng = false)]]
SEXP rcpp_bc_strrep_bv(
  SEXP x, SEXP y,  bool bigx, SEXP out_dim,
  R_xlen_t nout
) {

const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);

MACRO_DIM_BIG2VECTOR(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));


return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_strrep_d, rng = false)]]
SEXP rcpp_bc_strrep_d(
  SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout
) {


const SEXP *px = STRING_PTR_RO(x);
const int *py = INTEGER_RO(y);

CharacterVector out(nout);

MACRO_DIM_DOCALL(out[flatind_out] = rcpp_string_mult(px[flatind_x], py[flatind_y]));

return out;

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
readr::write_file(txt, "src/rcpp_bc_strrep.cpp")

