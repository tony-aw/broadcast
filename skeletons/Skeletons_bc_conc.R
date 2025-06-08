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


txt0 <- "


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_string_conc)]]
String rcpp_string_conc(
    String x, String y, String sep
  ) {
    if(x == NA_STRING || y == NA_STRING) {
      return(NA_STRING);
    }
    else {
      String out = \"\";
      out += x;
      out += sep;
      out += y;
      return(out);
    }
}

"


txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_conc)]]
void rcpp_bc_conc(
  CharacterVector x, SEXP y, String sep,
  SEXP by_y, SEXP dcp_y, SEXP x_dim
) {

const SEXP *py = STRING_PTR_RO(y);

MACRO_DIM_SET_DOCALL(x[flatind_x] = rcpp_string_conc(x[flatind_x], py[flatind_y], sep));


}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt0, txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)



setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_conc.cpp")


# test
x <- matrix("", 5, 5)
y <- array("hello", c(1, 5))
sep <- " MYSEP "
by_y <- broadcast:::.C_make_by(dim(y))
dcp_y <- broadcast:::.C_make_dcp(dim(y))
.rcpp_bc_conc(x, y, "", by_y, dcp_y, dim(x))
x
.rcpp_bc_conc(x, y, sep, by_y, dcp_y, dim(x))
x
y
