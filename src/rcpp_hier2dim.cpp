
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


inline void rcpp_rec_n(
  SEXP x, SEXP count, int depth, int maxdepth, bool recurse_classed
) {
  double maxlen = pow(2, 31) - 1;
  R_xlen_t n = Rf_xlength(x);
  if(n > maxlen) {
    stop("long vectors not supported");
  }
  for(int i = 0; i < n; ++i) {
    SEXP temp = VECTOR_ELT(x, i);
    if(rcpp_OK_listclass(temp, recurse_classed) && (Rf_xlength(temp) > 0) && (depth < maxdepth)) {
      rcpp_rec_n(temp, count, depth + 1, maxdepth, recurse_classed);
    }
    else {
      SET_REAL_ELT(count, 0, REAL(count)[0] + 1);
    }
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_hier_flatlen)]]
SEXP rcpp_hier_flatlen(
  SEXP x, int maxdepth, bool recurse_classed
) {
  SEXP count = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(count)[0] = 0;
  rcpp_rec_n(x, count, 1, maxdepth, recurse_classed);
  UNPROTECT(1);
  return count;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rec_depths)]]
void rcpp_rec_depths(
  SEXP x, SEXP out, SEXP index, int depth, int maxdepth, bool recurse_classed
) {
  double maxlen = pow(2, 31) - 1;
  R_xlen_t n = Rf_xlength(x);
  if(n > maxlen) {
    stop("long vectors not supported");
  }
  double *pout = REAL(out);
  if(n > 0) {
    for(int i = 0; i < n; ++i) {
      SEXP temp = VECTOR_ELT(x, i);
      R_xlen_t index0 = REAL(index)[0];
      if(rcpp_OK_listclass(temp, recurse_classed) && (Rf_xlength(temp) > 0) && (depth < maxdepth)) {
        rcpp_rec_depths(temp, out, index, depth + 1, maxdepth, recurse_classed);
      }
      else {
        pout[index0] = depth;
        REAL(index)[0] = index0 + 1;
      }
    }
  }
  
}


inline void rcpp_rec_len(
  SEXP x, SEXP out, SEXP index, int depth, int depth_target, bool recurse_classed
) {
  
  double maxlen = pow(2, 31) - 1;
  R_xlen_t n = Rf_xlength(x);
  if(n > maxlen) {
    stop("long vectors not supported");
  }
  
  for(int i = 0; i < n; ++i) {
    
    SEXP temp = VECTOR_ELT(x, i);
    
    if(depth == depth_target) {
      R_xlen_t n_temp = Rf_xlength(temp);
      if(n_temp > maxlen) {
        stop(" long vectors not supported");
      }
      SET_INTEGER_ELT(out, REAL(index)[0], n_temp);
      SET_REAL_ELT(index, 0, REAL(index)[0] + 1);
    }
    else if(rcpp_OK_listclass(temp, recurse_classed) && depth != depth_target && Rf_xlength(temp) != 0) {
      rcpp_rec_len(temp, out, index, depth + 1, depth_target, recurse_classed);
    }
    
  }
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_hierlen)]]
SEXP rcpp_hierlen(
  SEXP x, int depth_target, R_xlen_t n, bool recurse_classed
) {
  SEXP index = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(index)[0] = 0;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  
  rcpp_rec_len(x, out, index, 1, depth_target, recurse_classed);
  UNPROTECT(2);
  return out;
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



