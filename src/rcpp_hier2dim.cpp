#include <Rcpp/Lightest>
using namespace Rcpp;


inline bool rcpp_OK_listclass(
  SEXP x, bool recurse_classed
) {
  if(TYPEOF(x) != VECSXP) {
    return false;
  }
  if(recurse_classed) {
    return true;
  }
  return (Rf_getAttrib(x, R_ClassSymbol) == R_NilValue);
}


inline void rcpp_rec_depth_range(
  SEXP x, SEXP range, int depth, int depth_limit, bool recurse_classed, double maxint
) {
  R_xlen_t n = Rf_xlength(x);
  if(n >= maxint) {
    stop("long vectors not supported");
  }
  int *prange = INTEGER(range);
  if(n > 0) {
    for(int i = 0; i < n; ++i) {
      SEXP temp = VECTOR_ELT(x, i);
      if(rcpp_OK_listclass(temp, recurse_classed) && (Rf_xlength(temp) > 0) && (depth < depth_limit)) {
        rcpp_rec_depth_range(temp, range, depth + 1, depth_limit, recurse_classed, maxint);
      }
      else {
        if(depth < prange[0]) {
          prange[0] = depth;
        }
        if(depth > prange[1]) {
          prange[1] = depth;
        }
        
      }
    }
  }
  
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_depth_range)]]
SEXP rcpp_depth_range(SEXP x, int depth_limit, bool recurse_classed) {
  
  
  double maxint = pow(2, 31) - 1;
  int depth = 1;
  
  SEXP range = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(range)[0] = maxint;
  INTEGER(range)[1] = 0;
  
  rcpp_rec_depth_range(x, range, depth, depth_limit, recurse_classed, maxint);
  
  UNPROTECT(1);
  return range;
  
}


inline void rcpp_rec_lenrange_atdepth(
  SEXP x, SEXP range, int depth, int depth_target, bool recurse_classed, double maxint
) {
  R_xlen_t n = Rf_xlength(x);
  if(n >= maxint) {
    stop("long vectors not supported");
  }
  int *prange = INTEGER(range);
  if(n > 0) {
    for(int i = 0; i < n; ++i) {
      SEXP temp = VECTOR_ELT(x, i);
      R_xlen_t n_temp = Rf_xlength(temp);
      if(!rcpp_OK_listclass(temp, recurse_classed)) {
        continue;
      }
      if(depth == depth_target) {
        if(n_temp > maxint) {
          stop("long vectors not supported");
        }
        if(n_temp < prange[0]) {
          prange[0] = n_temp;
        }
        if(n_temp > prange[1]) {
          prange[1] = n_temp;
        }
      }
      else if(n_temp > 0 && (depth != depth_target)) {
        rcpp_rec_lenrange_atdepth(temp, range, depth + 1, depth_target, recurse_classed, maxint);
      }
    }
  }
  
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_lenrange_atdepth)]]
SEXP rcpp_lenrange_atdepth(SEXP x, int depth_target, bool recurse_classed) {
  
  
  double maxint = pow(2, 31) - 1;
  int depth = 1;
  
  SEXP range = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(range)[0] = maxint;
  INTEGER(range)[1] = 0;
  
  rcpp_rec_lenrange_atdepth(x, range, depth, depth_target, recurse_classed, maxint);
  
  UNPROTECT(1);
  return range;
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_hier2dim_surface_OK)]]
bool rcpp_hier2dim_surface_OK(
  SEXP x, bool recurse_classed
) {

  double maxlen = pow(2, 31) - 1;
  R_xlen_t n = Rf_xlength(x);
  if(n > maxlen) {
    stop("long vectors not supported");
  }
  
  for(int i = 0; i < n; ++i) {
    SEXP temp = VECTOR_ELT(x, i);
    if(!rcpp_OK_listclass(temp, recurse_classed) || (Rf_xlength(temp) == 0)) {
      return false;
    }
  }
  return true;
}


inline void rcpp_rec_dropnests(
  SEXP x, SEXP out, SEXP index, int depth, int maxdepth, bool recurse_classed
) {
  double n = Rf_xlength(x);
  for(R_xlen_t i = 0; i < n; ++i) {
    SEXP temp = VECTOR_ELT(x, i);
    if(rcpp_OK_listclass(temp, recurse_classed) && Rf_xlength(temp) == 1 && (depth < maxdepth)) {
      rcpp_rec_dropnests(temp, out, index, depth + 1, maxdepth, recurse_classed);
    }
    else {
      SET_VECTOR_ELT(out, REAL(index)[0], temp);
      REAL(index)[0] = REAL(index)[0] + 1;
    }
  }
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_dropnests)]]
SEXP rcpp_dropnests(
  SEXP x, int maxdepth, bool recurse_classed
) {
   SEXP index = PROTECT(Rf_allocVector(REALSXP, 1));
   REAL(index)[0] = 0;
   SEXP out = PROTECT(Rf_allocVector(VECSXP, Rf_xlength(x)));
   rcpp_rec_dropnests(x, out, index, 1, maxdepth, recurse_classed);
   UNPROTECT(2);
   return out;
}



