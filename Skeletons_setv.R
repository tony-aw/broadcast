# set-up ====

library(stringi)

SXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")

header <- "

#include <Rcpp/lightest>

using namespace Rcpp;

"


################################################################################

# set vind 32 bit ====

templatecode <- "

template<int RTYPE> void rcpp_set_vind_32_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const int *pind = INTEGER_RO(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
}


"


switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_vind_32_template<TYPESXP>(as<RCPPTYPEVector>(x), ind, as<RCPPTYPEVector>(rp));
    break;
  }
"

switches <- character(6L)

for(i in 1:6) {
  switches[i] <- stringi::stri_replace_all_fixed(
    switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SXPTYPES[i], RCPPTYPES[i]),
    vectorize_all = FALSE
  )
}

switches <- stringi::stri_paste(switches, collapse = "\n")
cat(switches)



code32 <- stri_c(
  
  templatecode,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_atomic)]]
void rcpp_set_vind_32_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
  default: stop(\"unsupported type given\");
}
}
"
)

cat(code32)


################################################################################

# set vind 64 bit ====


templatecode <- "

template<int RTYPE> void rcpp_set_vind_64_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const double *pind = REAL_RO(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
}


"


switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_vind_64_template<TYPESXP>(as<RCPPTYPEVector>(x), ind, as<RCPPTYPEVector>(rp));
    break;
  }
"

switches <- character(6L)

for(i in 1:6) {
  switches[i] <- stringi::stri_replace_all_fixed(
    switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SXPTYPES[i], RCPPTYPES[i]),
    vectorize_all = FALSE
  )
}

switches <- stringi::stri_paste(switches, collapse = "\n")
cat(switches)


code64 <- stri_c(
  
  templatecode,
  
  
  "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_atomic)]]
void rcpp_set_vind_64_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
  default: stop(\"unsupported type given\");
}
}
"
)

cat(code64)


################################################################################
# Combine set vind ====

code <- stri_paste(
  header,
  code32,
  code64,
  collapse = "\n \n"
)


cat(code)

Rcpp::sourceCpp(code = code)

fileConn <- file("src/rcpp_set_vind.cpp")
writeLines(code, fileConn)
close(fileConn)

