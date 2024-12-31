# set-up ====

library(stringi)
# dMacro_skeletons <- qs::qread("dMacro_skeletons")
# macros <- stri_c(dMacro_skeletons, collapse = "\n")


macro_dim_1_8 <- readr::read_file("macro_dim_1_8.txt")
macro_dim_16 <- readr::read_file("macro_dim_16.txt")
macro_dim_docall <- readr::read_file("macro_dim_docall.txt")
macro_typeswitch_complex <- readr::read_file("macro_typeswitch_complex.txt")
macro_dim_general <- readr::read_file("macro_dim_general.txt")

header <- stri_c("

#include <Rcpp.h>

using namespace Rcpp;
",
  macro_dim_1_8,
  "\n",
  macro_dim_16,
  "\n",
  macro_dim_docall,
  "\n",
  macro_dim_general,
  "\n",
  macro_typeswitch_complex,
  "\n",
  
  "
  
  
  //' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_test)]]
int rcpp_test(int x, int y) {
  return (x + y);
}
  
  "
)

cat(header)
cat(stringi::stri_replace_all(header, "", fixed = "\\") )
readr::write_file(header, "header.txt")

Rcpp::sourceCpp(code = header)


Rcpp::cppFunction(
  "
  SEXP complex_plus(
    SEXP x, SEXP y
  ) {
    R_xlen_t n = Rf_xlength(x);
    Rcomplex *px = COMPLEX(x);
    Rcomplex *py = COMPLEX(y);
    SEXP out = PROTECT(Rf_allocVector(CPLXSXP, n));
    Rcomplex *pout;
    pout = COMPLEX(out);
    
    
    for(int i = 0; i < n; ++i) {
      pout[i] = px[i] + py[i];
    }
    
    UNPROTECT(1);
    return out;
  }
  "
)

gen <- function() {
  sample(c(1:10, NA, NaN, Inf, -Inf))
}

x <- gen() + gen() * -1i
y <- gen() + gen() * -1i

tinytest::expect_equal(
  x + y,
  complex_plus(x, y)
)


Rcpp::cppFunction(
  "
  SEXP complex_plus(
    SEXP x, SEXP y
  ) {
    R_xlen_t n = Rf_xlength(x);
    Rcomplex *px = COMPLEX(x);
    double *py = REAL(y);
    SEXP out = PROTECT(Rf_allocVector(CPLXSXP, n));
    Rcomplex *pout;
    pout = COMPLEX(out);
    
    
    for(int i = 0; i < n; ++i) {
      pout[i] = px[i] + as<Rcomplex>(py[i]);
    }
    
    UNPROTECT(1);
    return out;
  }
  "
)

gen <- function() {
  sample(c(1:10, NA, NaN, Inf, -Inf))
}

x <- gen() + gen() * -1i
y <- gen() + gen() * -1i

tinytest::expect_equal(
  x + y,
  complex_plus(x, y)
)

################################################################################
# dfunction skeleton ====
#

txt <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_cplx_d)]]
SEXP rcpp_bcD_cplx_d(
  SEXP x, SEXP y,
  List sub_x,
  List sub_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, R_xlen_t nout, int op
) {


double *pdim_x;
pdim_x = REAL(dimcumprod_x);
double *pdim_y;
pdim_y = REAL(dimcumprod_y);

R_xlen_t flatind_x;
R_xlen_t flatind_y;

Rcomplex tempout;

SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
Rcomplex *pout;
pout = COMPLEX(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC(
      MACRO_DIM_DOCALL,
      tempout.r = NA_REAL; tempout.i = NA_REAL,
      tempout = (Rcomplex)px[flatind_x] + (Rcomplex)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = px[flatind_x] - py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = px[flatind_x] * py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (Rcomplex)px[flatind_x] / (Rcomplex)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = pow((Rcomplex)px[flatind_x], (Rcomplex)py[flatind_y])
    );
    break;
  }
}

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(header, txt, collapse = "\n\n")
Rcpp::sourceCpp(code = txt)


################################################################################
# gfunction skeleton ====
#

# THIS WORKS :-)

txt <- "


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_cplx_general)]]
SEXP rcpp_bcD_cplx_general(
  SEXP x, SEXP y,
  const SEXP s1, const SEXP s2,
  const SEXP xdims1, const SEXP xdims2, const R_xlen_t nout, int op
) {
  
  Rcomplex tempout;

  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));
  Rcomplex *pout;
  pout = REAL(out);
  
  switch(op) {
    case 1:
    {
      MACRO_TYPESWITCH_NUMERIC(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (Rcomplex)px[flatind_x] + (Rcomplex)py[flatind_y]
      );
      break;
    }
    case 2:
    {
      MACRO_TYPESWITCH_NUMERIC(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (Rcomplex)px[flatind_x] - (Rcomplex)py[flatind_y]
      );
      break;
    }
    case 3:
    {
      MACRO_TYPESWITCH_NUMERIC(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (Rcomplex)px[flatind_x] * (Rcomplex)py[flatind_y]
      );
      break;
    }
    case 4:
    {
      MACRO_TYPESWITCH_NUMERIC(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (Rcomplex)px[flatind_x] / (Rcomplex)py[flatind_y]
      );
      break;
    }
    case 5:
    {
      MACRO_TYPESWITCH_NUMERIC(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = pow((Rcomplex)px[flatind_x], (Rcomplex)py[flatind_y])
      );
      break;
    }
  
  }
  
  
  UNPROTECT(1);
  return out;
  
  
}


"



txt <- stringi::stri_c(header, txt, collapse = "\n\n")
Rcpp::sourceCpp(code = txt)


################################################################################
# perform tests ====
#

x.dim <- c(50:48)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1L, c(1,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- cumprod(dim(x))
dimcumprod_y <- cumprod(dim(y))
out.dim <- pmax(dim(x), dim(y))
out.len <- prod(out.dim)
dimcumprod_out <- cumprod(out.dim)
inds_x <- lapply(out.dim, \(n)1:n)
inds_y <- lapply(out.dim, \(n)rep(1L, n))

expected <- as.vector(drop(y) / x)

out1 <- .rcpp_bcD_cplx_d(
  y, x, inds_y, inds_x,
  dimcumprod_y, dimcumprod_x, out.len, 4L
)
out2 <- .rcpp_bcD_cplx_general(
  y, x, inds_y, inds_x, dim(y), dim(x), out.len, 4L
)

tinytest::expect_equal(expected, out1)
tinytest::expect_equal(expected, out2)
tinytest::expect_equal(expected, outer(y, x, "/") |> as.vector())

px <- as.integer(x)
py <- as.integer(y)

foo <- bench::mark(
  base = as.vector(drop(y) / x),
  outer = outer(y, x, "/") |> as.vector(),
  bc_d = .rcpp_bcD_cplx_d(
    y, x, inds_y, inds_x,
    dimcumprod_y, dimcumprod_x, out.len, 4L
  ),
  bc_general = .rcpp_bcD_cplx_general(
    y, x, inds_y, inds_x, dim(y), dim(x), out.len, 4L
  ),
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)



# bigger benchmark

x.dim <- c(60:58)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1.1:50.1, c(60,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- cumprod(dim(x))
dimcumprod_y <- cumprod(dim(y))
out.dim <- pmax(dim(x), dim(y))
out.len <- prod(out.dim)
dimcumprod_out <- cumprod(out.dim)
inds_x <- lapply(out.dim, \(n) 1:n)
inds_y <- list(1:60, rep(1L, 59), rep(1L, 58))


out1 <- .rcpp_bcD_cplx_d(
  x, y, inds_x, inds_y,
  dimcumprod_x, dimcumprod_y, out.len, 4L
)
out2 <- .rcpp_bcD_cplx_general(
  x, y, inds_x, inds_y, dim(x), dim(y), out.len, 4L
)
tinytest::expect_equal(
  out1, out2
)


foo <- bench::mark(
  base = as.vector(x / drop(y)),
  outer = outer(x, y, "/") |> as.vector(),
  bc_d = .rcpp_bcD_cplx_d(
    x, y, inds_x, inds_y,
    dimcumprod_x, dimcumprod_y, out.len, 4L
  ),
  bc_general = .rcpp_bcD_cplx_general(
    x, y, inds_x, inds_y, dim(x), dim(y), out.len, 4L
  ),
  check = FALSE, # because base and outer wouldn't work correctly here
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)


