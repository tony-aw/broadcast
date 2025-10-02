#include <Rcpp/Lightest>
using namespace Rcpp;


inline bool rcpp_OK_listclass(
  SEXP x, bool recurse_all
) {
  if(TYPEOF(x) != VECSXP) {
    return false;
  }
  if(recurse_all) {
    return true;
  }
  
  bool check_class = Rf_getAttrib(x, R_ClassSymbol) == R_NilValue;
  bool check_dim = Rf_getAttrib(x, R_DimSymbol) == R_NilValue;
  return (check_class && check_dim);
}

inline void rcpp_rec_depth_range(
  SEXP x, SEXP range, int depth, int depth_limit, bool recurse_all, double maxint
) {
  R_xlen_t n = Rf_xlength(x);
  if(n >= maxint) {
    stop("long vectors not supported");
  }
  int *prange = INTEGER(range);
  if(n > 0) {
    for(int i = 0; i < n; ++i) {
      SEXP temp = VECTOR_ELT(x, i);
      if(rcpp_OK_listclass(temp, recurse_all) && (Rf_xlength(temp) > 0) && (depth < depth_limit)) {
        rcpp_rec_depth_range(temp, range, depth + 1, depth_limit, recurse_all, maxint);
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
SEXP rcpp_depth_range(SEXP x, int depth_limit, bool recurse_all) {
  
  
  double maxint = pow(2, 31) - 1;
  int depth = 1;
  
  SEXP range = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(range)[0] = maxint;
  INTEGER(range)[1] = 0;
  
  rcpp_rec_depth_range(x, range, depth, depth_limit, recurse_all, maxint);
  
  UNPROTECT(1);
  return range;
  
}


inline void rcpp_rec_lenrange_atdepth(
  SEXP x, SEXP range, int depth, int depth_target, bool recurse_all, double maxint
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
      if(!rcpp_OK_listclass(temp, recurse_all)) {
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
        rcpp_rec_lenrange_atdepth(temp, range, depth + 1, depth_target, recurse_all, maxint);
      }
    }
  }
  
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_lenrange_atdepth)]]
SEXP rcpp_lenrange_atdepth(SEXP x, int depth_target, bool recurse_all) {
  
  
  double maxint = pow(2, 31) - 1;
  int depth = 1;
  
  SEXP range = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(range)[0] = maxint;
  INTEGER(range)[1] = 0;
  
  rcpp_rec_lenrange_atdepth(x, range, depth, depth_target, recurse_all, maxint);
  
  UNPROTECT(1);
  return range;
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_hier2dim_surface_OK)]]
bool rcpp_hier2dim_surface_OK(
  SEXP x, bool recurse_all
) {

  double maxlen = pow(2, 31) - 1;
  R_xlen_t n = Rf_xlength(x);
  if(n > maxlen) {
    stop("long vectors not supported");
  }
  
  for(int i = 0; i < n; ++i) {
    SEXP temp = VECTOR_ELT(x, i);
    if(!rcpp_OK_listclass(temp, recurse_all) || (Rf_xlength(temp) == 0)) {
      return false;
    }
  }
  return true;
}


inline void rcpp_rec_dropnests(
  SEXP x, SEXP out, SEXP index, int depth, int maxdepth, bool recurse_all
) {
  double n = Rf_xlength(x);
  for(R_xlen_t i = 0; i < n; ++i) {
    SEXP temp = VECTOR_ELT(x, i);
    if(rcpp_OK_listclass(temp, recurse_all) && Rf_xlength(temp) == 1 && (depth < maxdepth)) {
      rcpp_rec_dropnests(temp, out, index, depth + 1, maxdepth, recurse_all);
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
  SEXP x, int maxdepth, bool recurse_all
) {
   SEXP index = PROTECT(Rf_allocVector(REALSXP, 1));
   REAL(index)[0] = 0;
   SEXP out = PROTECT(Rf_allocVector(VECSXP, Rf_xlength(x)));
   rcpp_rec_dropnests(x, out, index, 1, maxdepth, recurse_all);
   UNPROTECT(2);
   return out;
}



inline void rcpp_rec_firstnames_atdepth(
  SEXP x, SEXP out, int len_target, int depth, int depth_target, bool recurse_all, double maxint
) {
  if(VECTOR_ELT(out, 0) != R_NilValue) {
    return;
  }
  R_xlen_t n = Rf_xlength(x);
  if(n >= maxint) {
    stop("long vectors not supported");
  }
  if(n > 0) {
    for(int i = 0; i < n; ++i) {
      SEXP temp = VECTOR_ELT(x, i);
      R_xlen_t n_temp = Rf_xlength(temp);
      if(!rcpp_OK_listclass(temp, recurse_all)) {
        continue;
      }
      else if(depth == depth_target) {
        if(n_temp > maxint) {
          stop("long vectors not supported");
        }
        if(n_temp != len_target || Rf_getAttrib(temp, R_NamesSymbol) == R_NilValue) {
          continue;
        }
        SET_VECTOR_ELT(out, 0, Rf_getAttrib(temp, R_NamesSymbol));
        return;
      }
      else if(n_temp > 0 && (depth < depth_target)) {
        rcpp_rec_firstnames_atdepth(temp, out, len_target, depth + 1, depth_target, recurse_all, maxint);
      }
    }
  }
  
}



inline void rcpp_rec_lastnames_atdepth(
  SEXP x, SEXP out, int len_target, int depth, int depth_target, bool recurse_all, double maxint
) {
  if(VECTOR_ELT(out, 0) != R_NilValue) {
    return;
  }
  R_xlen_t n = Rf_xlength(x);
  if(n >= maxint) {
    stop("long vectors not supported");
  }
  if(n > 0) {
    for(int i = (n-1); i >= 0; --i) {
      SEXP temp = VECTOR_ELT(x, i);
      R_xlen_t n_temp = Rf_xlength(temp);
      if(!rcpp_OK_listclass(temp, recurse_all)) {
        continue;
      }
      else if(depth == depth_target) {
        if(n_temp > maxint) {
          stop("long vectors not supported");
        }
        if(n_temp != len_target || Rf_getAttrib(temp, R_NamesSymbol) == R_NilValue) {
          continue;
        }
        SET_VECTOR_ELT(out, 0, Rf_getAttrib(temp, R_NamesSymbol));
        return;
      }
      else if(n_temp > 0 && (depth < depth_target)) {
        rcpp_rec_lastnames_atdepth(temp, out, len_target, depth + 1, depth_target, recurse_all, maxint);
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_names_atdepth)]]
SEXP rcpp_names_atdepth(SEXP x, int direction, int len_target, int depth_target, bool recurse_all) {
  
  double maxint = pow(2, 31) - 1;
  int depth = 1;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  
  if(direction == 1) {
    rcpp_rec_firstnames_atdepth(x, out, len_target, depth, depth_target - 1, recurse_all, maxint);
  }
  else if(direction == -1) {
    rcpp_rec_lastnames_atdepth(x, out, len_target, depth, depth_target - 1, recurse_all, maxint);
  }
  else {
    stop("argument `direction` incorrectly specified");
  }
  
  UNPROTECT(1);
  return out;
  
}


