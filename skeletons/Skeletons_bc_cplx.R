# set-up ====
source("SOURCE.R")

################################################################################
# Inlines ====
#

inlines <- "


inline Rcomplex inline_cplx_plus( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  out.r = x.r + y.r;
  out.i = x.i + y.i;
  return out;
}


inline Rcomplex inline_cplx_min( const Rcomplex& x, const Rcomplex& y) {
  
  Rcomplex out;
  
  out.r = x.r - y.r ;
  out.i = x.i - y.i ;
  return out;
}


inline Rcomplex inline_cplx_mult( const Rcomplex& x, const Rcomplex& y) {
  Rcomplex out;
  std::complex<double> cx(x.r, x.i);
  std::complex<double> cy(y.r, y.i);
  std::complex<double> res = cx * cy;
  out.r = res.real();
  out.i = res.imag();
  return out;
}



inline Rcomplex inline_cplx_div( const Rcomplex& x, const Rcomplex& y) {
  Rcomplex out;
  std::complex<double> cx(x.r, x.i);
  std::complex<double> cy(y.r, y.i);
  std::complex<double> res = cx / cy;
  out.r = res.real();
  out.i = res.imag();
  return out;

}


"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#



macro_op_cplx_math <- "
#define MACRO_OP_CPLX_MATH(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_plus(tempx, tempy), \\
      const Rcomplex tempx = px[flatind_x], \\
      const Rcomplex tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_min(tempx, tempy), \\
      const Rcomplex tempx = px[flatind_x], \\
      const Rcomplex tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_mult(tempx, tempy), \\
      const Rcomplex tempx = px[flatind_x], \\
      const Rcomplex tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_div(tempx, tempy), \\
      const Rcomplex tempx = px[flatind_x], \\
      const Rcomplex tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_cplx_math, collapse = "\n"))


################################################################################
# Functions ====
#




prep <- "
Rcomplex *pout = COMPLEX(out);

const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);
"

txt1 <- generate_functions(
  "bc_cplx", "ComplexVector", prep, "MACRO_OP_CPLX_MATH"
)
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macro_op_cplx_math,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macro_op_cplx_math,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_cplx.cpp")

