
#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_setnames)]]
void rcpp_bindhelper_setnames(
  SEXP x, const SEXP ind, SEXP rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const int *pind = INTEGER_RO(ind);
  const SEXP *prp = STRING_PTR_RO(rp);
  
  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(x, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(x, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
}



inline int rcpp_bindhelper_conf_dims_2(
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
