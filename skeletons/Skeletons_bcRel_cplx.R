# set-up ====

library(stringi)

source("SOURCE.R")

################################################################################
# Inlines ====
#


inlines <- "


inline int inline_cplx_equal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r == y.r && x.i == y.i) {
    return(1);
  }
  else return(0);
}


inline int inline_cplx_unequal(
    const Rcomplex& x, const Rcomplex& y
) {
  if(std::isnan(x.r) || std::isnan(x.i) || std::isnan(y.r) || std::isnan(y.i)) {
    return(NA_LOGICAL);
  }
  else if(x.r != y.r || x.i != y.i) {
    return(1);
  }
  else return(0);
}


"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))



################################################################################
# MACROs ====
#



macro_op_cplx_rel <- "
#define MACRO_OP_CPLX_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_equal(tempx, tempy),  \\
      const Rcomplex tempx = px[flatind_x], \\
      const Rcomplex tempy = py[flatind_y]  \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = inline_cplx_unequal(tempx, tempy),  \\
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


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_cplx_rel, collapse = "\n"))


################################################################################
# Functions ====
#


prep <- "
const Rcomplex *px = COMPLEX(x);
const Rcomplex *py = COMPLEX(y);
int *pout = LOGICAL(out);
"

txt1 <- generate_functions(
  "bcRel_cplx", "LogicalVector", prep, "MACRO_OP_CPLX_REL"
)

cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macro_op_cplx_rel,
   txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macro_op_cplx_rel,
   txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcRel_cplx.cpp")

