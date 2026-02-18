
#include <Rcpp/Lightest>
using namespace Rcpp;




inline bool rcpp_binames_consider_dim(SEXP outdims, SEXP xdims, SEXP xdimnames ) {
  
  if(xdimnames == R_NilValue) {
    return false;
  }
  
  int n = Rf_length(xdims);
  int *pxdims = INTEGER(xdims);
  int *poutdims = INTEGER(outdims);
  
  for(int i = 0; i < n; ++i) {
    if(pxdims[i] == poutdims[i] && VECTOR_ELT(xdimnames, i) != R_NilValue) {
      return true;
    }
  }
  return false;
}


inline bool rcpp_binames_consider_flat(RObject target, R_xlen_t out_len) {
  return (Rf_xlength(target) == out_len) && (target.attr("names") != R_NilValue);
}





//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_dimnames_fit)]]
 bool rcpp_dimnames_fit(
     RObject x, SEXP dimnames
 ) {
   
   if(!x.hasAttribute("dim")) {
     return false;
   }
   
   SEXP xdim = x.attr("dim");
   if(Rf_length(xdim) == 0) {
     return false;
   }
   
   if(dimnames == R_NilValue) {
     return true;
   }
   
   if(Rf_length(dimnames) != Rf_length(xdim)) {
     return false;
   }
   
   if(TYPEOF(dimnames) != VECSXP) {
     return false;
   }
   
   int n = Rf_length(xdim);
   const int *pxdim = INTEGER_RO(xdim);
   SEXP temp;
   for(int i = 0; i < n; ++i) {
     temp = VECTOR_ELT(dimnames, i);
     if(temp != R_NilValue && Rf_length(temp) != pxdim[i]) {
       return false;
     }
   }
   
   return true;
 }






inline SEXP rcpp_vector_elt(
    SEXP x, int i, int len
) {
  if(i >= len) {
    return R_NilValue;
  }
  else {
    return VECTOR_ELT(x, i);
  }
}


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_make_dimnames2)]]
 SEXP rcpp_make_dimnames2(
     SEXP xdimnames, SEXP ydimnames, SEXP outdim, int pref
 ) {
   
   
   int n = Rf_length(outdim);
   SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
   int *poutdim = INTEGER(outdim);
   bool checkx;
   bool checky;
   int lenx = Rf_length(xdimnames);
   int leny = Rf_length(ydimnames);
   
   
   for(int i = 0; i < n; ++i) {
     
     SEXP tempx = rcpp_vector_elt(xdimnames, i, lenx);
     SEXP tempy = rcpp_vector_elt(ydimnames, i, leny);
     checkx = Rf_length(tempx) == poutdim[i];
     checky = Rf_length(tempy) == poutdim[i];
     
     
     if(!checkx && !checky) {
       continue;
     }
     
     
     if(checkx && checky) {
       if(std::addressof(tempx) == std::addressof(tempy)) {
         SET_VECTOR_ELT(out, i, tempx);
       }
       else if(pref == 1) {
         SET_VECTOR_ELT(out, i, tempx);
       }
       else if(pref == 2) {
         SET_VECTOR_ELT(out, i, tempy);
       }
       else {
         continue;
       }
     }
     else if(checkx) {
       SET_VECTOR_ELT(out, i, tempx);
     }
     else if(checky) {
       SET_VECTOR_ELT(out, i, tempy);
     }
     
   }
   
   UNPROTECT(1);
   return out;
 }



//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_make_dimnames_between)]]
 SEXP rcpp_make_dimnames_between(
     SEXP adimnames, SEXP vnames, SEXP outdim
 ) {
   
   
   int n = Rf_length(outdim);
   SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
   int *poutdim = INTEGER(outdim);
   bool checka, checkv;
   int lena = Rf_length(adimnames);
   
   
   // rows:
   checkv = Rf_length(vnames) == poutdim[0];
   SEXP tempa = VECTOR_ELT(adimnames, 0);
   checka = Rf_length(tempa) == poutdim[0];
   if(checka && checkv) {
     SET_VECTOR_ELT(out, 0, tempa);
   }
   else if(checka) {
     SET_VECTOR_ELT(out, 0, tempa);
   }
   else if(checkv) {
     SET_VECTOR_ELT(out, 0, vnames);
   }
   
   
   // other dims:
   if(lena == 1) {
     UNPROTECT(1);
     return out;
   }
   for(int i = 0; i < lena; ++i) {
     SEXP tempa = VECTOR_ELT(adimnames, i);
     checka = Rf_length(tempa) == poutdim[i];
     if(checka) {
       SET_VECTOR_ELT(out, i, tempa);
     }
   }
   
   UNPROTECT(1);
   return out;
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_make_dimnames1)]]
 SEXP rcpp_make_dimnames1(
     SEXP y_dimnames, SEXP ydim, SEXP outdim
 ) {
   
   SEXP out = PROTECT(Rf_allocVector(VECSXP, Rf_length(outdim)));
   
   int *pydim = INTEGER(ydim);
   int *poutdim = INTEGER(outdim);
   
   for(int i = 0; i < Rf_length(ydim); ++i) {
     if(pydim[i] == poutdim[i]) {
       SEXP temp = VECTOR_ELT(y_dimnames, i);
       SET_VECTOR_ELT(out, i, temp);
     }
   }
   
   UNPROTECT(1);
   
   return out;
 }



inline void rcpp_binames_set_flat(RObject x, RObject y, RObject out) {
  R_xlen_t out_len = Rf_xlength(out);
  
  bool checkx = rcpp_binames_consider_flat(x, out_len);
  bool checky = rcpp_binames_consider_flat(y, out_len);
  
  if(!checkx && !checky) {
    return;
  }
  
  if(checkx && checky) {
    // `x` and `y` conflict; use names of `x`:
    out.attr("names") = x.attr("names");
    return;
    
  }
  
  if(checkx) { // use `x`
    out.attr("names") = x.attr("names");
    return;
  }
  
  
  if(checky) { // use `y`
    out.attr("names") = y.attr("names");
    return;
  }
  
  
  return;
  
}




inline void rcpp_binames_set_between(
    RObject x, RObject y, RObject out, SEXP out_dim
) {
  
  RObject v;
  RObject a;
  if(x.attr("dim") == R_NilValue) {
    v = x;
    a = y;
  }
  else {
    v = y;
    a = x;
  }
  
  SEXP v_names = v.attr("names");
  SEXP a_dimnames = a.attr("dimnames");
  SEXP a_dim = a.attr("dim");
  
  bool checkv = (v_names != R_NilValue) && (Rf_length(v) == INTEGER(out_dim)[0]);
  bool checka = rcpp_binames_consider_dim(out_dim, a_dim, a_dimnames);
  
  
  if(!checkv && !checka) {
    return;
  }
  
  if(checkv && checka) {
    // consider both `v` and `a`
    List out_dimnames = rcpp_make_dimnames_between(a_dimnames, v_names, out_dim);
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  } // end consider both `a` and `v`
  
  
  if(checkv) {
    // consider only `v`
    List out_dimnames(Rf_length(out_dim));
    out_dimnames[0] = v_names;
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  } // end consider only `v`
  
  
  if(checka) {
    // consider only `a`
    List out_dimnames = rcpp_make_dimnames1(a_dimnames, a_dim, out_dim);
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  } // end consider only `a`
  
  
  // else:
  return;
  
}





inline void rcpp_binames_set_dim(
    RObject x, RObject y, RObject out, SEXP out_dim
) {
  
  // Prep:
  SEXP x_dimnames = x.attr("dimnames");
  SEXP y_dimnames = y.attr("dimnames");
  SEXP x_dim = x.attr("dim");
  SEXP y_dim = y.attr("dim");
  bool checkx = rcpp_binames_consider_dim(out_dim, x_dim, x_dimnames);
  bool checky = rcpp_binames_consider_dim(out_dim, y_dim, y_dimnames);
  
  if(!checkx && !checky) {
    return;
  }
  
  if(checkx && checky){
    int x_ndim = Rf_length(x_dim);
    int y_ndim = Rf_length(y_dim);
    R_xlen_t x_len = Rf_xlength(x);
    R_xlen_t y_len = Rf_xlength(y);
    int pref;
    if(x_len > y_len && x_ndim > y_ndim) {
      pref = 1;
    }
    else if(y_len > x_len && y_ndim > x_ndim) {
      pref = 2;
    }
    else {
      pref = 0;
    }
    List out_dimnames = rcpp_make_dimnames2(x_dimnames, y_dimnames, out_dim, pref);
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  }
  
  if(checkx) {
    List out_dimnames = rcpp_make_dimnames1(x_dimnames, x_dim, out_dim);
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  }
  
  if(checky) {
    List out_dimnames = rcpp_make_dimnames1(y_dimnames, y_dim, out_dim);
    if(rcpp_dimnames_fit(out, out_dimnames)) {
      out.attr("dimnames") = out_dimnames;
    }
    return;
  }
  
  // else:
  return;
  
}




//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_binames_set)]]
 void rcpp_binames_set(RObject x, RObject y, RObject out) {
   if(Rf_xlength(out) == 0) {
     return;
   }
   
   SEXP out_dim = out.attr("dim");
   
   if(Rf_length(out_dim) <= 1) {
     rcpp_binames_set_flat(x, y, out);
     return;
   }
   else if(x.hasAttribute("dim") != y.hasAttribute("dim")) {
     rcpp_binames_set_between(x, y, out, out_dim);
     return;
   }
   else {
     rcpp_binames_set_dim(x, y, out, out_dim);
     return;
   }
   
 }
