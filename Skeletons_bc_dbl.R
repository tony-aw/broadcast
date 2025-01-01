# set-up ====

library(stringi)

macro_dim <- readr::read_file("macro_dim.txt")
macro_typeswitch_numeric <- readr::read_file("macro_typeswitch_numeric.txt")
macro_action <- readr::read_file("macro_action.txt")

header_for_sourcing <- stri_c(
  "
  #include <Rcpp.h>
  
  using namespace Rcpp;
  ",
  macro_action,
  "\n",
  macro_dim,
  "\n",
  macro_typeswitch_numeric,
  "\n"
)


header_for_package <- "

#include <Rcpp.h>
#include \"Macros_Everywhere.h\"

using namespace Rcpp;


"

################################################################################
# Functions ====
#


txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_v)]]
SEXP rcpp_bc_dbl_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_VECTOR,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop(\"given operator not supported in the given context\");
  }
}

UNPROTECT(1);
return out;

}


"


txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_bs)]]
SEXP rcpp_bc_dbl_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_BIGSMALL_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop(\"given operator not supported in the given context\");
  }
}

UNPROTECT(1);
return out;

}


"


txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_o)]]
SEXP rcpp_bc_dbl_o(
  SEXP x, SEXP y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, bool xstarts,
  int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_ORTHO_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop(\"given operator not supported in the given context\");
  }
}

UNPROTECT(1);
return out;

}


"


txt4 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_d)]]
SEXP rcpp_bc_dbl_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop(\"given operator not supported in the given context\");
  }
}

UNPROTECT(1);
return out;

}


"

txt5 <- "


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_general)]]
SEXP rcpp_bc_dbl_general(
  SEXP x, SEXP y,
  const SEXP s1, const SEXP s2,
  const SEXP xdims1, const SEXP xdims2, const R_xlen_t nout, int op
) {
  
  double tempout;

  SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
  double *pout;
  pout = REAL(out);
  
  switch(op) {
    case 1:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] + (double)py[flatind_y]
      );
      break;
    }
    case 2:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] - (double)py[flatind_y]
      );
      break;
    }
    case 3:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] * (double)py[flatind_y]
      );
      break;
    }
    case 4:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] / (double)py[flatind_y]
      );
      break;
    }
    case 5:
    {
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(
        MACRO_DIM_GENERAL,
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
        tempout = 1,
        tempout = NA_REAL,
        tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
      );
      break;
    }
    case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
    default:
    {
      stop(\"given operator not supported in the given context\");
    }
  
  }
  
  
  UNPROTECT(1);
  return out;
  
  
}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt1, txt2, txt3, txt4, txt5,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

txt <- stringi::stri_c(
  header_for_package,
  txt1, txt2, txt3, txt4, txt5,
  collapse = "\n\n"
)
readr::write_file(txt, "src/bc_dbl.cpp")


################################################################################
# perform tests ====
#
library(tinytest)
n <- 100
gen <- function(n)sample(c(1, NA, NaN, Inf, -Inf), n, TRUE)
x <- array(gen(n), c(n, 1))
y <- array(gen(n), c(1, n))
dimcumprod_x <- c(1, cumprod(dim(x)))
dimcumprod_y <- c(1, cumprod(dim(y)))
out.dim <- pmax(dim(x), dim(y)) |> as.integer()
out.len <- prod(out.dim)
out.dcp <- cumprod(out.dim)
by_x <- c(1L, 0L)
by_y <- c(0L, 1L)
inds_x <- list(1:(out.dim[1]), rep(1L, out.dim[2]))
inds_y <- rev(inds_x)

expected <- as.vector(x[, rep(1L, n)] - y[rep(1L, n), ])
out1 <- .rcpp_bc_dbl_d(
  x, y, by_x, by_y, dimcumprod_x, dimcumprod_y, out.dim, out.len, 2L
)
out2 <- .rcpp_bc_dbl_general(
  x, y, inds_x, inds_y, dim(x), dim(y), out.len, 2L
)
expect_equal(
  expected, out1
)
expect_equal(
  expected, out2
)

foo <- bench::mark(
  d = .rcpp_bc_dbl_d(
    x, y, by_x, by_y, dimcumprod_x, dimcumprod_y, out.dim, out.len, 1L
  ),
  g = .rcpp_bc_dbl_general(
    x, y, inds_x, inds_y, dim(x), dim(y), out.len, 1L
  ),
  min_iterations = 100
)
summary(foo)
ggplot2::autoplot(foo)


x.dim <- c(5:3)
x.len <- prod(x.dim)
x.data <- gen(x.len)
x <- array(x.data, x.dim)
y <- array(1L, c(1,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- c(1, cumprod(dim(x)))
dimcumprod_y <- c(1, cumprod(dim(y)))
out.dim <- pmax(dim(x), dim(y)) |> as.integer()
out.len <- prod(out.dim)
dimcumprod_out <- c(1, cumprod(out.dim))
by_x <- rep(1L, length(out.dim))
by_y <- rep(0L, length(out.dim))
inds_x <- lapply(out.dim, \(n)1:n)
inds_y <- lapply(out.dim, \(n)rep(1L, n))

expected <- as.vector(drop(y) - x)

out1 <- .rcpp_bc_dbl_d(
  y, x, by_y, by_x,
  dimcumprod_y, dimcumprod_x, out.dim, out.len, 2L
)
out2 <- .rcpp_bc_dbl_general(
  y, x, inds_y, inds_x, dim(y), dim(x), out.len, 2L
)
out3 <- .rcpp_bc_dbl_v(y, x, out.len, 2L)

tinytest::expect_equal(expected, out1)
tinytest::expect_equal(expected, out2)
tinytest::expect_equal(expected, out3)
tinytest::expect_equal(expected, outer(y, x, "-") |> as.vector())

px <- as.integer(x)
py <- as.integer(y)

foo <- bench::mark(
  base = as.vector(drop(y) / x),
  outer = outer(y, x, "/") |> as.vector(),
  bc_d = .rcpp_bc_dbl_d(
    y, x, by_y, by_x,
    dimcumprod_y, dimcumprod_x, out.dim, out.len, 4L
  ),
  bc_general = .rcpp_bc_dbl_general(
    y, x, inds_y, inds_x, dim(y), dim(x), out.len, 4L
  ),
  bc_vector = .rcpp_bc_dbl_v(y, x, out.len, 4L),
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)



# bigger benchmark

x.dim <- c(100:98)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1:50, c(100,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- cumprod(dim(x))
dimcumprod_y <- cumprod(dim(y))
out.dim <- pmax(dim(x), dim(y)) |> as.integer()
out.len <- prod(out.dim)
dimcumprod_out <- cumprod(out.dim)
inds_x <- lapply(out.dim, \(n) 1:n)
inds_y <- list(1:100, rep(1L, 99), rep(1L, 98))
by_x <- rep(1L, length(out.dim))
by_y <- c(1L, 0L, 0L)


out1 <- .rcpp_bc_dbl_d(
  x, y, by_x, by_y,
  dimcumprod_x, dimcumprod_y, out.dim, out.len, 4L
)
out2 <- .rcpp_bc_dbl_general(
  x, y, inds_x, inds_y, dim(x), dim(y), out.len, 4L
)
out3 <- .rcpp_bc_dbl_v(x, y, out.len, 4L)
tinytest::expect_equal(
  out1, out2
)


foo <- bench::mark(
  base = as.vector(x / drop(y)),
  bc_d = .rcpp_bc_dbl_d(
    x, y, by_x, by_y,
    dimcumprod_x, dimcumprod_y, out.dim, out.len, 4L
  ),
  bc_general = .rcpp_bc_dbl_general(
    x, y, inds_x, inds_y, dim(x), dim(y), out.len, 4L
  ),
  check = FALSE, # because base wouldn't work correctly here
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)


