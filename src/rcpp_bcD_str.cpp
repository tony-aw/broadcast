

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;





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




#define MACRO_OP_STR_DIST(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_str_dist_led(tempx, tempy),  \
      const String tempx = px[flatind_x], \
      const String tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_str_dist_lcss(tempx, tempy),  \
      const String tempx = px[flatind_x], \
      const String tempy = py[flatind_y]  \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)

  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcD_str_2v, rng = false)]]
  SEXP rcpp_bcD_str_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  IntegerVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout  = INTEGER(out);


  
  MACRO_OP_STR_DIST(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcD_str_1v, rng = false)]]
  SEXP rcpp_bcD_str_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  IntegerVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout  = INTEGER(out);


  
  MACRO_OP_STR_DIST(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bcD_str_d, rng = false)]]
  SEXP rcpp_bcD_str_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  IntegerVector out(no_init(nout));
  
  

const SEXP *px = STRING_PTR_RO(x);
const SEXP *py = STRING_PTR_RO(y);
int *pout  = INTEGER(out);


  
  MACRO_OP_STR_DIST(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
