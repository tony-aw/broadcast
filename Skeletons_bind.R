
################################################################################

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

#include <Rcpp.h>
#include \"Broadcast.h\"

using namespace Rcpp;


"

# readr::write_file(header_for_sourcing, "header.txt")

Rcpp::sourceCpp(code = header_for_sourcing)



################################################################################
# Functions ====
#



txt <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(rcpp_bc_bind)]]
void rcpp_bc_bind(
  SEXP out, SEXP x,
  SEXP starts, SEXP ends, SEXP by_x,
  SEXP dimcumprod_out, SEXP dimcumprod_x, SEXP out_dim
) {


double *pout = REAL(out);
double *px = REAL(x);

MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

txt <- stringi::stri_c(
  header_for_package,
  txt,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_bind.cpp")


################################################################################
# perform tests ====
#
library(tinytest)

# rbind:
## make args and allocate output:
margin <- 1L
x <- matrix(as.double(1:4), ncol = 2)
y <- matrix(5:6, ncol = 1)
args <- list(x, y)
out.dim <- do.call(pmax, lapply(args, dim))
out.dim[margin] <- out.dim[margin] * length(args)
out <- array(NA_real_, dim = out.dim)

## make params:
dcp_x <- c(1, cumprod(dim(x)))
dcp_y <- c(1, cumprod(dim(y)))
dcp_out <- c(1, cumprod(dim(out)))
by_x <- ifelse(dim(x) > 1L, 1L, 0L)
by_y <- ifelse(dim(y) > 1L, 1L, 0L)

## do part 1:
print(out)
starts <- c(1L, 1L)
ends <- c(nrow(x), ncol(out))
rcpp_bc_bind(out, x, starts, ends, by_x, dcp_out, dcp_x, out.dim)
print(out)


# NOTE:
# broadcasting in binding should NOT be the same as broadcasting in binary operations
# For example:
# given dim(x) = c(10, 10) and dim(y) = c(10, 1),
# y would be broadcasted to dim(y) = c(10, 10).
# but when cbinding, dim(y) should remain c(10, 1)
# I think in general the case is,
# that the dimension along which you bind should NOT broadcast...
# In general, I conclude that broadcasting while binding is a bad idea...



################################################################################
# quick benchmark

library(tinytest)
library(broadcast)

.internal_bind_array <- broadcast:::.internal_bind_array


n <- 75
x <- array(as.double(1:25), c(n, n, n))
y <- array(as.double(-1:-25), c(n, n, n))
input <- list(x, y, x, y, x, y)

out1 <- abind::abind(input, along = 1L)
out2 <- .internal_bind_array(input, 1L, sys.call())
expect_equivalent(
  out1, out2
)

foo <- bench::mark(
  abind = abind::abind(input, along = 2L),
  bc = .internal_bind_array(input, 2L, sys.call()),
  cbind = do.call(cbind, input),
  min_iterations = 100,
  check = FALSE # because abind adds empty dimnames
)
summary(foo)
ggplot2::autoplot(foo)


