# set-up ====

library(stringi)

header <- "

#include <Rcpp/Lightest>
  
  using namespace Rcpp;

"


################################################################################
# MACROs ====
#


macro_checkmissing_typeswitch <- "
#define MACRO_CHECKMISSING_TYPESWITCH(MACROCODE, DOCODE) do { \\
  switch(TYPEOF(y)) {                                                         \\
    case INTSXP:                                                              \\
    case LGLSXP:                                                              \\
    {                                                                         \\
      const int *py = INTEGER_RO(y);                                          \\
      MACROCODE(DOCODE, py[i] == NA_INTEGER, py[i] != NA_INTEGER);            \\
      break;                                                                  \\
    }                                                                         \\
    case REALSXP:                                                             \\
    {                                                                         \\
      const double *py = REAL_RO(y);                                          \\
      MACROCODE(DOCODE, R_isnancpp(py[i]), !R_isnancpp(py[i]));               \\
      break;                                                                  \\
    }                                                                         \\
    case CPLXSXP:                                                             \\
    {                                                                         \\
      const Rcomplex *py = COMPLEX_RO(y);                                     \\
      MACROCODE(                                                              \\
        DOCODE,                                                               \\
        R_isnancpp(py[i].r) || R_isnancpp(py[i].i),                           \\
        !R_isnancpp(py[i].i) && !R_isnancpp(py[i].r)                          \\
      );                                                                      \\
      break;                                                                  \\
    }                                                                         \\
    case STRSXP:                                                              \\
    {                                                                         \\
      const SEXP *py = STRING_PTR_RO(y);                                      \\
      MACROCODE(DOCODE, py[i] == NA_STRING, py[i] != NA_STRING);              \\
      break;                                                                  \\
    }                                                                         \\
    case VECSXP:                                                              \\
    {                                                                         \\
      MACROCODE(                                                              \\
        DOCODE,                                                               \\
        VECTOR_ELT(y, i) == R_NilValue,                                       \\
        VECTOR_ELT(y, i) != R_NilValue                                        \\
      );                                                                      \\
      break;                                                                  \\
    }                                                                         \\
    case RAWSXP:                                                              \\
    {                                                                         \\
      stop(\"NAs not defined for type `raw`\");                               \\
      break;                                                                  \\
    }                                                                         \\
    default: stop(\"Unsupported type\");                                      \\
  }                                                                           \\
} while(0)
"

macro_checkmissing_is <- "
#define MACRO_CHECKMISSING_IS(DOCODE, ISNACODE, ISNOTNACODE) do {  \\
  if(!invert) {                                                         \\
    for(R_xlen_t i = 0; i < n; ++i) {                                     \\
      DOCODE = ISNACODE;                                                    \\
    }                                                                     \\
  }                                                                       \\
  else if(invert) {                                                   \\
    for(R_xlen_t i = 0; i < n; ++i) {                                     \\
      DOCODE = ISNOTNACODE;                                              \\
    }                                                                     \\
  }                                                                       \\
  else {                                                                  \\
    stop(\"`invert` must be `TRUE` or `FALSE`\");                         \\
  }                                                                       \\
  break;                                                                  \\
} while(0)


"


macro_checkmissing_fw <- "
#define MACRO_CHECKMISSING_FW(DOCODE, ISNACODE, ISNOTNACODE) do {             \\
  if(!invert) {                                                         \\
    for(R_xlen_t i = 0; i < n; ++i) {                                     \\
      if(ISNACODE) {                                                      \\
        DOCODE;                                                           \\
      }                                                                   \\
    }                                                                     \\
  }                                                                       \\
  else if(invert) {                                                      \\
    for(R_xlen_t i = 0; i < n; ++i) {                                     \\
      if(ISNOTNACODE) {                                                   \\
        DOCODE;                                                           \\
      }                                                                   \\
    }                                                                     \\
  }                                                                       \\
  else {                                                                  \\
    stop(\"`invert` must be `TRUE` or `FALSE`\");                         \\
  }                                                                       \\
} while(0)


"

macro_checkmissing_bw <- stringi::stri_replace_all(
  macro_checkmissing_fw,
  "R_xlen_t i = (n-1); i >= 0; --i",
  fixed = "R_xlen_t i = 0; i < n; ++i"
)

macro_checkmissing_bw <- stringi::stri_replace_first(
  macro_checkmissing_bw,
  "MACRO_CHECKMISSING_BW",
  fixed = "MACRO_CHECKMISSING_FW"
)
cat(macro_checkmissing_bw)


macro_checkmissing <- paste0(
  macro_checkmissing_typeswitch,
  "\n",
  macro_checkmissing_is,
  "\n",
  macro_checkmissing_fw,
  "\n",
  macro_checkmissing_bw
)
cat(macro_checkmissing)

Rcpp::sourceCpp(code = paste0(header, "\n", macro_checkmissing))

readr::write_file(macro_checkmissing, "macro_checkmissing.txt")
