# set-up ====

library(stringi)

macro_dim <- readr::read_file("txt/macro_dim.txt")
inlines <- readr::read_file("txt/inlines.txt")



header_for_package <- "

#include <Rcpp/Lightest>
#include \"broadcast.h\"

using namespace Rcpp;


"


header_for_sourcing <- stri_c(
  "
  #include <Rcpp/Lightest>
  
  using namespace Rcpp;
  ",
  "\n",
  inlines,
  "\n",
  macro_dim
)

cat(header_for_sourcing)


Rcpp::sourceCpp(code = header_for_sourcing)


generate_functions <- function(basename, VectorType, prep, MACRO, additional_args = "") {
  
  txt1 <- "
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_<basename>_2v, rng = false)]]
  SEXP rcpp_<basename>_2v(
    SEXP x, SEXP y, List prep, int op <additional_args>
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  <VectorType> out(no_init(nout));
  
  <prep>
  
  <MACRO>(MACRO_DIM_TWOVECTORS);
  
  out.attr(\"dim\") = out_dimorig;
  
  return out;
  
  }
  
  
  "
  txt1 <- stringi::stri_replace_all(
    txt1, c(basename, VectorType, prep, MACRO, additional_args),
    fixed = c("<basename>", "<VectorType>", "<prep>", "<MACRO>", "<additional_args>"),
    vectorize_all = FALSE
  )
  
  
  txt2 <- "
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_<basename>_1v, rng = false)]]
  SEXP rcpp_<basename>_1v(
    SEXP x, SEXP y, List prep, int op <additional_args>
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  <VectorType> out(no_init(nout));
  
  <prep>
  
  <MACRO>(MACRO_DIM_ONEVECTOR);
  
  out.attr(\"dim\") = out_dimorig;
  
  return out;
  
  }
  "
  
  txt2 <- stringi::stri_replace_all(
    txt2, c(basename, VectorType, prep, MACRO, additional_args),
    fixed = c("<basename>", "<VectorType>", "<prep>", "<MACRO>", "<additional_args>"),
    vectorize_all = FALSE
  )
  
  
  
  txt3 <- "
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_<basename>_d, rng = false)]]
  SEXP rcpp_<basename>_d(
    SEXP x, SEXP y, List prep, int op <additional_args>
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  <VectorType> out(no_init(nout));
  
  <prep>
  
  <MACRO>(MACRO_DIM_DOCALL);
  
  out.attr(\"dim\") = out_dimorig;
  
  return out;
  
  }
  
  
  "
  
  
  txt3 <- stringi::stri_replace_all(
    txt3, c(basename, VectorType, prep, MACRO, additional_args),
    fixed = c("<basename>", "<VectorType>", "<prep>", "<MACRO>", "<additional_args>"),
    vectorize_all = FALSE
  )
  
  
  if(VectorType %in% c("NumericVector", "ComplexVector", "CharacterVector", "StringVector")) {
    txt2 <- ""
  }
  
  out <- stringi::stri_c(txt1, txt2, txt3, collapse = "\n\n")
  return(out)

}

