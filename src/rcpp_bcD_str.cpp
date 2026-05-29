

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;





inline int rcpp_str_dist_led(String x, String y) {
  
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


int rcpp_str_dist_lcss(String x, String y) {

    if(x == NA_STRING || y == NA_STRING) {
      return NA_INTEGER;
    }
  
    std::string s1 = x;
    std::string s2 = y;

    int m = s1.length();
    int n = s2.length();

    std::vector<int> prev(n + 1, 0);
    
    int res = 0;
    for (int i = 1; i <= m; i++) {
      
      std::vector<int> curr(n + 1, 0);
      
      for (int j = 1; j <= n; j++) {
      
        if (s1[i - 1] == s2[j - 1]) {
            curr[j] = prev[j - 1] + 1;
            res = std::max(res, curr[j]);
        } else {
            curr[j] = 0;
        }
        
      }
      prev = curr;
    }
    
    return res;
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_str_v, rng = false)]]
SEXP rcpp_bcD_str_v(
  SEXP x, SEXP y, SEXP x_dim, SEXP y_dim, SEXP out_dim,
  R_xlen_t nout, int dimmode, bool vectorx, int op
) {


const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
int *pout;
pout = INTEGER(out);

MACRO_OP_STR_DIST(MACRO_DIM_VECTORSPECIAL);


UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcD_str_d, rng = false)]]
SEXP rcpp_bcD_str_d(
  SEXP x, SEXP y, 
  SEXP by_x,
  SEXP by_y,
  SEXP dcp_x, SEXP dcp_y, SEXP out_dim, R_xlen_t nout, int op
) {

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);

SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));
int *pout;
pout = INTEGER(out);

MACRO_OP_STR_DIST(MACRO_DIM_DOCALL);

UNPROTECT(1);
return out;

}


