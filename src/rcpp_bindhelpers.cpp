
#include <Rcpp/Lightest>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_max_type)]]
int rcpp_bindhelper_max_type(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  int out = 1;
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    if(TYPEOF(tempout) == VECSXP) {
      out = 8;
    }
    else if(TYPEOF(tempout) == STRSXP && out < 8) {
      out = 7;
    }
    else if(TYPEOF(tempout) == CPLXSXP && out < 7) {
      out = 6;
    }
    else if(TYPEOF(tempout) == REALSXP && out < 6) {
      out = 5;
    }
    else if(TYPEOF(tempout) == INTSXP && out < 5) {
      out = 4;
    }
    else if(TYPEOF(tempout) == LGLSXP && out < 4) {
      out = 3;
    }
    else if(TYPEOF(tempout) == RAWSXP && out < 3) {
      out = 2;
    }
    else if(out < 2){
      out = 1;
    }
  }
  
  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_vdims)]]
SEXP rcpp_bindhelper_vdims(
    SEXP x
) {
  int n = Rf_length(x);
  SEXP tempout;
  SEXP tempdim;
  
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  
  for(int i = 0; i < n; ++i) {
    tempout = VECTOR_ELT(x, i);
    tempdim = Rf_getAttrib(tempout, R_DimSymbol);
    SET_VECTOR_ELT(out, i, tempdim);
  }
  
  UNPROTECT(1);
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_sum_along)]]
R_xlen_t rcpp_bindhelper_sum_along(
    SEXP lst_dims, int along
) {
  int n = Rf_length(lst_dims);
  SEXP tempdim;
  R_xlen_t out = 0;
  for(int i = 0; i < n; ++i) {
    tempdim = VECTOR_ELT(lst_dims, i);
    out += INTEGER(tempdim)[along];
  }
  
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_conf_dims_2)]]
int rcpp_bindhelper_conf_dims_2(
  SEXP x, SEXP y, int along, int max_bc
) {
  if(Rf_length(x) != Rf_length(y)) {
    return -1;
  }
  int n = Rf_length(x);
  int count_bc = 0;
  const int *px = INTEGER_RO(x);
  const int *py = INTEGER_RO(y);
  for(int i = 0; i < n; ++i) {
    if(i != along) {
      if(px[i] != py[i]) {
        if(px[i] != 1 && py[i] != 1) {
          return -1;
        }
        if(px[i] == 1 || py[i] == 1) {
          count_bc++;
        }
      }
    }
  }
  return count_bc;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_conf_dims_all)]]
int rcpp_bindhelper_conf_dims_all(
  SEXP lst_dims, SEXP target, int along, int max_bc
) {

  int n = Rf_length(lst_dims);
  int conf;
  int out = 0;
  SEXP tempout;
  for(int i = 0; i< n; ++i) {
    tempout = VECTOR_ELT(lst_dims, i);
    conf = rcpp_bindhelper_conf_dims_2(target, tempout, along, max_bc);
    if(conf < 0 ) {
      return -1;
    }
    if(conf > out) {
      out = conf;
    }
  }
  return out;
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_get_dimnames)]]
List rcpp_bindhelper_get_dimnames(
  List x, int along
) {
  int n = x.length();
  List out(n);
  for(int i = 0; i < n; ++i) {
    RObject temp = x[i];
    if(temp.hasAttribute("dimnames")) {
      List temp2 = temp.attr("dimnames");
      out[i] = temp2[along - 1];
    }
  }
  return out;
}
