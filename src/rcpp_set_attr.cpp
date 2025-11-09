

#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_attr)]]
void rcpp_set_attr(
    RObject x, String name, RObject value
  ) {
    x.attr(name) = value;
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
