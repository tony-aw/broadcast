
#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_max_type)]]
String rcpp_bindhelper_max_type(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  String out;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    if(TYPEOF(tempout) == VECSXP) {
      out = "list";
    }
    else if(TYPEOF(tempout) == STRSXP) {
      out = "character";
    }
    else if(TYPEOF(tempout) == CPLXSXP) {
      out = "complex";
    }
    else if(TYPEOF(tempout) == REALSXP) {
      out = "double";
    }
    else if(TYPEOF(tempout) == INTSXP) {
      out = "integer";
    }
    else if(TYPEOF(tempout) == LGLSXP) {
      out = "logical";
    }
    else if(TYPEOF(tempout) == RAWSXP) {
      out = "raw";
    }
    else {
      out = "unknown";
    }
  }
  
  return out;
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_max_dimlen)]]
int rcpp_bindhelper_max_dimlen(
  SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  SEXP tempdim;
  int tempdimlen;
  int out = 0;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    tempdimlen = Rf_length(tempdim);
    if(tempdimlen > out) {
      out = tempdimlen;
    }
  }
  
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_neednorm)]]
SEXP rcpp_bindhelper_neednorm(
  SEXP x, int target_dimlen
) {
  int n = Rf_length(x);
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *pout;
  pout = LOGICAL(out);
  
  SEXP tempout;
  SEXP tempdim;
  int tempdimlen;
  
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    tempdimlen = Rf_length(tempdim);
    if(tempdimlen < target_dimlen) {
      pout[i] = 1;
    }
    else {
      pout[i] = 0;
    }
  }
  
  UNPROTECT(1);
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_sum_along)]]
R_xlen_t rcpp_bindhelper_sum_along(
  SEXP x, int along
) {
  int n = Rf_length(x);
  SEXP tempout;
  SEXP tempdim;
  R_xlen_t out = 0;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    out += INTEGER(tempdim)[along];
  }
  
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_conf_dims_2)]]
bool rcpp_bindhelper_conf_dims_2(
  SEXP x, SEXP y, int along
) {
  if(Rf_length(x) != Rf_length(y)) {
    return false;
  }
  int n = Rf_length(x);
  int count_bc = 0;
  const int *px = INTEGER_RO(x);
  const int *py = INTEGER_RO(y);
  for(int i = 0; i < n; ++i) {
    if(i != along) {
      if(px[i] != py[i]) {
        if(px[i] != 1 && py[i] != 1) {
          return false;
        }
        if(px[i] == 1 || py[i] == 1) {
          count_bc++;
        }
      }
    }
  }
  if(count_bc > 1) {
    return false;
  }
  return true;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_conf_dims_all)]]
bool rcpp_bindhelper_conf_dims_all(
  SEXP lst_dims, SEXP target, int along
) {
  
  int n = Rf_length(lst_dims);
  bool is_conf;
  SEXP tempout;
  for(int i = 0; i< n; ++i) {
    tempout = VECTOR_ELT(lst_dims, i);
    is_conf = rcpp_bindhelper_conf_dims_2(target, tempout, along);
    if(!is_conf) {
      return false;
    }
  }
  return true;
}

