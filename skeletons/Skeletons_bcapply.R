# set-up ====

library(stringi)
macro_dim <- readr::read_file("txt/macro_dim.txt")
inlines <- readr::read_file("txt/inlines.txt")
header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  using namespace Rcpp;
  ",
  "\n",
  inlines,
  "\n",
  macro_dim,
  "\n"
)
header_for_package <- "
#include <Rcpp/Lightest>
#include \"broadcast.h\"
using namespace Rcpp;
"
Rcpp::sourceCpp(code = header_for_sourcing)




################################################################################
# MACROs ====
#


macro_op_bcapply <- "
#define MACRO_OP_BCAPPLY(DIMCODE) do {       \\
  switch(TYPEOF(out)) {	\\
    case LGLSXP:	\\
    {	\\
      int *pout;	\\
      pout = LOGICAL(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = LOGICAL_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \\
      );	\\
      break; \\
    }	\\
    case INTSXP:	\\
    {	\\
      int *pout;	\\
      pout = INTEGER(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = INTEGER_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \\
      );	\\
      break; \\
    }	\\
    case REALSXP:	\\
    {	\\
      double *pout;	\\
      pout = REAL(out);	\\
      DIMCODE(	\\
        pout[flatind_out] = REAL_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,   \\
      );	\\
      break; \\
    }	\\
    case CPLXSXP:	\\
    {	\\
      Rcomplex *pout;	\\
      pout = COMPLEX(out);	\\
      DIMCODE(	\\
        pout[flatind_out] = COMPLEX_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \\
      );	\\
      break; \\
    }	\\
    case STRSXP:	\\
    {	\\
      	\\
      DIMCODE(	\\
        SET_STRING_ELT(out, flatind_out, STRING_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0)), ,   \\
      );	\\
      break; \\
    }	\\
    case RAWSXP:	\\
    {	\\
      Rbyte *pout;	\\
      pout = RAW(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = RAW_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0), ,    \\
      );	\\
      break; \\
    }	\\
    case VECSXP:	\\
    {	\\
     DIMCODE(	\\
        SET_VECTOR_ELT(out, flatind_out, f(x, y, flatind_x + 1, flatind_y + 1)), ,    \\
      );	\\
      break; \\
    }	\\
    default:	\\
    {	\\
      stop(\"unsupported type\");	\\
    }	\\
  }	\\
} while(0)
"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macro_op_bcapply, collapse = "\n"))



################################################################################
# Functions ====
#




txt1 <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_2v, rng = false)]]
void rcpp_bcapply_2v(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_TWOVECTORS);

}


"

txt2 <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_1v, rng = false)]]
void rcpp_bcapply_1v(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_ONEVECTOR);

}


"


txt3 <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcapply_d, rng = false)]]
void rcpp_bcapply_d(
  SEXP out, SEXP x, SEXP y, List prep, Function f
) {

  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

MACRO_OP_BCAPPLY(MACRO_DIM_DOCALL);

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  macro_op_bcapply,
  txt1, txt2, txt3,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macro_op_bcapply,
  txt1, txt2, txt3,
  collapse = "\n\n"
)

readr::write_file(txt, "src/rcpp_bcapply.cpp")

