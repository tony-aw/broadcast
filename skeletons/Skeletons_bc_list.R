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
// [[Rcpp::export(.rcpp_bc_list_v)]]
SEXP rcpp_bc_list_v(
  List x, List y, 
  R_xlen_t nout, Function f
) {


List out(nout);

MACRO_DIM_VECTOR(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);


return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_ov)]]
SEXP rcpp_bc_list_ov(
  List x, List y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, Function f
) {

List out(nout);

MACRO_DIM_ORTHOVECTOR(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);



return out;

}


"

txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_bs)]]
SEXP rcpp_bc_list_bs(
  List x, List y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  Function f
) {


List out(nout);

MACRO_DIM_BIGSMALL_DOCALL(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);


return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_list_d)]]
SEXP rcpp_bc_list_d(
  List x, List y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, Function f
) {


List out(nout);

MACRO_DIM_DOCALL(
  out[flatind_out] = f(x[flatind_x], y[flatind_y])
);

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
readr::write_file(txt, "src/rcpp_bc_list.cpp")

