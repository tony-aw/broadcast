
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

#include <Rcpp/Lightest>
#include \"broadcast.h\"

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
// [[Rcpp::export(.rcpp_bc_bind_prep)]]
void rcpp_bc_bind_prep(
  SEXP starts, SEXP ends, SEXP by_x, SEXP dcp_x, SEXP dim_x, SEXP dim_out, int along, int size_along, int counter, int n
) {

  // get properties:
  const int *pdim_out = INTEGER_RO(dim_out);
  const int *pdim_x = INTEGER_RO(dim_x);
  
  // check lens:
  if(Rf_length(starts) != n) stop(\"improper length for `starts`\");
  if(Rf_length(ends) != n) stop(\"improper length for `ends`\");
  if(Rf_length(by_x) != n) stop(\"improper length for `by_x`\");
  if(Rf_length(dim_x) != n) stop(\"improper length for `dim_x`\");
  if(Rf_length(dim_out) != n) stop(\"improper length for `dim_out`\");
  if(Rf_length(dcp_x) != (n+1)) stop(\"improper length for `dcp_x`\");
  
  // starts:
  int *pstart = INTEGER(starts);
  for(int i = 0; i < n; ++i) {
    pstart[i] = 0;
  }
  pstart[along] = counter;
  
  
  // ends:
  int *pend = INTEGER(ends);
  for (int i = 0; i < n; ++i) {
    pend[i] = pdim_out[i] - 1;
  }
  pend[along] = counter + size_along - 1;
  
  
  // by_x:
  int *pby_x = INTEGER(by_x);
  for(int i = 0; i < n; ++i) {
    pby_x[i] = pdim_x[i] > 1 ? 1 : 0;
  }
  pby_x[along] = 1;
  
  
  // dcp_x:
  double *pdcp_x = REAL(dcp_x);
  double temp_prod = pdim_x[0];
  pdcp_x[0] = 1;
  pdcp_x[1] = pdim_x[0];
  if((n+1) > 2) {
    for(int i = 2; i < (n+1); ++i) {
      temp_prod = temp_prod * pdim_x[i-1];
      pdcp_x[i] = temp_prod;
    }
  }
  
  
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_bind)]]
void rcpp_bc_bind(
  SEXP out, SEXP x,
  SEXP starts, SEXP ends, SEXP by_x,
  SEXP dcp_out, SEXP dcp_x, SEXP out_dim
) {


  switch(TYPEOF(out)) {
    case RAWSXP:
    {
      Rbyte *pout = RAW(out);
      Rbyte *px = RAW(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case LGLSXP:
    case INTSXP:
    {
      int *pout = INTEGER(out);
      int *px = INTEGER(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case REALSXP:
    {
      double *pout = REAL(out);
      double *px = REAL(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case CPLXSXP:
    {
      Rcomplex *pout = COMPLEX(out);
      Rcomplex *px = COMPLEX(x);
      MACRO_DIM_BIND_DOCALL(pout[flatind_out] = px[flatind_x]);
      break;
    }
    case STRSXP:
    {
      const SEXP *px = STRING_PTR_RO(x);
      MACRO_DIM_BIND_DOCALL(SET_STRING_ELT(out, flatind_out, px[flatind_x]));
      break;
    }
    case VECSXP:
    {
      MACRO_DIM_BIND_DOCALL(SET_VECTOR_ELT(out, flatind_out, VECTOR_ELT(x, flatind_x)));
      break;
    }
  }

}


"



txt_source <- stringi::stri_c(
  header_for_sourcing,
  txt,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt_source)

txt_pkg <- stringi::stri_c(
  header_for_package,
  txt,
  collapse = "\n\n"
)
setwd("..")
readr::write_file(txt_pkg, "src/rcpp_bc_bind.cpp")

