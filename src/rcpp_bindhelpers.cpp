
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



inline SEXP rcpp_rep_new_int(
    int n, int val
) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *pout = INTEGER(out);
  for(int i = 0; i < n; ++i) {
    pout[i] = val;
  }
  
  UNPROTECT(1);
  return(out);
}

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_bindhelper_make_input_dims)]]
 SEXP rcpp_bindhelper_make_input_dims(
     SEXP dims_old, int start, int max_ndim
 ) {
   int n = Rf_length(dims_old);
   
   SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
   
   for(int i = 0; i < n; ++i) {
     SEXP temp_new = rcpp_rep_new_int(max_ndim, 1);
     SEXP temp_old = Rf_getAttrib(VECTOR_ELT(dims_old, i), R_DimSymbol);
     
     int m = Rf_length(temp_old);
     
     int *pnew = INTEGER(temp_new);
     const int *pold = INTEGER_RO(temp_old);
     
     for(int j = 0; j < m; ++j) {
       pnew[j + start] = pold[j];
     }
     
     SET_VECTOR_ELT(out, i, temp_new);
     
   }
   
   UNPROTECT(1);
   return out;
   
   
 }


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bindhelper_anyinput_hasclass)]]
bool rcpp_bindhelper_anyinput_hasclass(
    SEXP input, String class2check
) {
  
  
  int n = Rf_length(input);
  RObject temp;
  
  for(int i  = 0; i < n; ++i) {
    temp = VECTOR_ELT(input, i);
    if(temp.hasAttribute("class")) {
      CharacterVector out_class = temp.attr("class");
      for(int j = 0; j < out_class.length(); ++j) {
        String current_class = out_class[j];
        if(current_class == class2check) {
          return true;
        }
      }
    }
    
  }
  
  return false;
  
  
}

