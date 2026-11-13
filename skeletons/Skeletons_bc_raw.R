# set-up ====

library(stringi)
source("SOURCE.R")


################################################################################
# Inlines ====
#

inlines <- "
inline Rbyte inline_raw_diff(Rbyte x, Rbyte y) {
  Rbyte out = (x > y) ? (x - y) : (y - x);
  return out;
}
"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#


macro_op_raw_byte <- "
#define MACRO_OP_RAW_BYTE(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx == tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx != tempy, \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx < tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx > tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx <= tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = tempx >= tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    } \\
    case 7:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = (tempx < tempy) ? tempx : tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 8:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = (tempx > tempy) ? tempx : tempy,   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    case 9:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = inline_raw_diff(tempx, tempy),   \\
        const Rbyte tempx = px[flatind_x],  \\
        const Rbyte tempy = py[flatind_y]   \\
      );                                                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"

Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_raw_byte, collapse = "\n"))


################################################################################
# Functions ====
#

prep <- "
Rbyte *pout = RAW(out);
Rbyte *px = RAW(x);
Rbyte *py = RAW(y);
"
txt1 <- generate_functions(
  "bc_raw", "RawVector", prep, "MACRO_OP_RAW_BYTE"
)
cat(txt1)


txt <- stringi::stri_c(
  header_for_sourcing,
  inlines, macro_op_raw_byte,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines, macro_op_raw_byte,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_raw.cpp")

