# set-up ====
library(stringi)
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "

inline double inline_dec_pmin(double x, double y) {
  const bool missing = inline_branchless_or(std::isnan(x), std::isnan(y));
  double out = missing ? NA_REAL : std::min(x, y);
  return out;
}


inline double inline_dec_pmax(double x, double y) {
  const bool missing = inline_branchless_or(std::isnan(x), std::isnan(y));
  double out = missing ? NA_REAL : std::max(x, y);
  return out;
}

"

################################################################################
# MACROS ====
#


macro_main_dec_p <- "

#define MACRO_MAIN_DEC_P(DIMCODE, DOCODE) do {  \\
  DIMCODE(                                      \\
      pout[flatind_out] = DOCODE,               \\
      const double tempx = px[flatind_x],       \\
      const double tempy = py[flatind_y]        \\
    );                                          \\
} while(0)


"




macro_op_dec_p <- "
#define MACRO_OP_DEC_P(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_MAIN_DEC_P(	        \\
        DIMCODE,                            \\
        inline_dec_pmin(tempx, tempy)  \\
      );	                                  \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_MAIN_DEC_P(	        \\
        DIMCODE,                            \\
        inline_dec_pmax(tempx, tempy)  \\
      );	                                  \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macros_dec_p <- paste0(
  macro_main_dec_p, macro_op_dec_p
)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_dec_p))


################################################################################
# Functions ====
#


prep <- "
const double *px = REAL_RO(x);
const double *py = REAL_RO(y);
double *pout = REAL(out);

"

txt1 <- generate_functions(
  "bcP_dec", "NumericVector", prep, "MACRO_OP_DEC_P"
)
cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_dec_p,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_dec_p,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcP_dec.cpp")

