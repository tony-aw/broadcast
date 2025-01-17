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
// [[Rcpp::export(.rcpp_bcapply_v)]]
void rcpp_bcapply_v(
  SEXP out, SEXP x, SEXP y, 
  R_xlen_t nout, Function f
) {

MACRO_OP_APPLY(MACRO_DIM_VECTOR);

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_ov)]]
void rcpp_bcapply_ov(
  SEXP out, SEXP x, SEXP y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, Function f
) {

MACRO_OP_APPLY(MACRO_DIM_ORTHOVECTOR);

}


"

txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_bs)]]
void rcpp_bcapply_bs(
  SEXP out, SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  Function f
) {


MACRO_OP_APPLY(MACRO_DIM_BIGSMALL_DOCALL);


}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_d)]]
void rcpp_bcapply_d(
  SEXP out, SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, Function f
) {


MACRO_OP_APPLY(MACRO_DIM_DOCALL);


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
readr::write_file(txt, "src/rcpp_bcapply.cpp")


# small test:
f <- function(x, y, ix, iy) {
  return(x[ix + 1] + y[iy + 1])
}
f2 <- function(x, y) x + y
n <- 3e4
x <- 1:n
y <- 1:n
f(x, y, 1, 1) |> typeof()
out <- integer(n)
print(out)
.rcpp_bcapply_v(out, x, y, n, f)
print(out)

expect_equal(
  mapply(f2, x, y),
  out
)


foo <- bench::mark(
  mapply(f2, x, y),
  .rcpp_bcapply_v(integer(n), x, y, n, f),
  min_iterations = 100,
  check = FALSE
)
summary(foo)
ggplot2::autoplot(foo)
