# set-up ====

library(stringi)

macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")

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


txt0 <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_string_plus)]]
String rcpp_string_plus(
    String x, String y
  ) {
    if(x == NA_STRING || y == NA_STRING) {
      return(NA_STRING);
    }
    else {
      String out = x;
      out += y;
      return(out);
    }
}

"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_v)]]
SEXP rcpp_bc_str_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {


CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);
MACRO_OP_STR_CONC(
  MACRO_DIM_VECTOR
);


return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_ov)]]
SEXP rcpp_bc_str_ov(
  SEXP x, SEXP y, bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {

CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_ORTHOVECTOR
);


return out;

}


"

txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_bs)]]
SEXP rcpp_bc_str_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {

CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_BIGSMALL_DOCALL
);

return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_str_d)]]
SEXP rcpp_bc_str_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {


CharacterVector px = as<CharacterVector>(x);
CharacterVector py = as<CharacterVector>(y);
CharacterVector out(nout);

MACRO_OP_STR_CONC(
  MACRO_DIM_DOCALL
);

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
readr::write_file(txt, "src/rcpp_bc_str.cpp")

