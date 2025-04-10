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
  macro_op,
  "\n"
)


header_for_package <- "

#include <Rcpp/Lightest>
#include \"broadcast.h\"

using namespace Rcpp;


"

readr::write_file(header_for_sourcing, "header.txt")

Rcpp::sourceCpp(code = header_for_sourcing)



################################################################################
# Functions ====
#


txt0 <- "


//' @keywords internal
//' @noRd
// [[Rcpp::export(rcpp_str_dist_led)]]
int rcpp_str_dist_led(String x, String y) {
  
  if(x == NA_STRING || y == NA_STRING) {
     return NA_INTEGER;
  }
  
  std::string s1 = x;
  std::string s2 = y;
  
  // Number of elements
  int n = s1.size();
  int m = s2.size();
  int nrow = n + 1;
  int ncol = m + 1;
  std::vector<int> d(nrow * ncol, 0);

  if (n == 0){
    return m;
  }

  if (m == 0){
    return n;
  }

  for (int i = 0; i < nrow; i++){
    d[i] = i;
  }

  for (int j = 1; j < ncol; j++){
    d[nrow * j] = j;
  }

  for (int j = 1; j <= m; j++){

    for (int i = 1; i <= n; i++){

      if (s1[i - 1] == s2[j - 1]){

        d[i + nrow * j] = d[(i - 1) + nrow * (j - 1)];  // no operation

      } else {

        d[i + nrow * j] = std::min(
          d[(i - 1) + nrow * j] + 1,    //a deletion
          std::min(
            d[i + nrow * (j - 1)] + 1,   //an insertion
            d[(i - 1) + nrow * (j - 1)] + 1
          )
        ); //a substitution

      } // end if

    } // end inner for

  } // end outer for

  return d[n + nrow * m];
}


"

txt1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_str_v)]]
SEXP rcpp_bcDist_str_v(
  CharacterVector x, CharacterVector y, 
  R_xlen_t nout, int op
) {




SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
int *pout;
pout = INTEGER(out);

MACRO_OP_STR_DIST(MACRO_DIM_VECTOR);


UNPROTECT(1);
return out;

}


"



txt2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_str_ov)]]
SEXP rcpp_bcDist_str_ov(
  CharacterVector x, CharacterVector y,  bool RxC, SEXP out_dim,
  R_xlen_t nout, int op
) {




SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
int *pout;
pout = INTEGER(out);

MACRO_OP_STR_DIST(MACRO_DIM_ORTHOVECTOR);

UNPROTECT(1);
return out;

}


"



txt3 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcDist_str_d)]]
SEXP rcpp_bcDist_str_d(
  CharacterVector x, CharacterVector y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {



SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
int *pout;
pout = INTEGER(out);

MACRO_OP_STR_DIST(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


"



txt <- stringi::stri_c(
  header_for_sourcing,
  txt0, txt1, txt2, txt3,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)

setwd("..")

txt <- stringi::stri_c(
  header_for_package,
  txt0, txt1, txt2, txt3,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bcDist_str.cpp")

