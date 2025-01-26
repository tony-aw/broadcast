#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_abind_all_conform_dims)]]
bool rcpp_abind_all_conform_dims(
  IntegerVector conform_dim, IntegerMatrix arg_dim, int n, int along
) {
  int m = arg_dim.nrow();
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < m; ++j) {
      if(j != (along - 1)) {
        if(conform_dim[j] != arg_dim(j, i)) {
          return false;
        }
      }
    }
  }
 return true;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_abind_get_maxdims)]]
IntegerVector rcpp_abind_get_maxdims(
  IntegerMatrix arg_dim
) {
  int n_dims = arg_dim.nrow();
  IntegerVector out(n_dims);
  for(int i = 0; i < n_dims; ++i) {
    IntegerMatrix::Row temp = arg_dim(i, _);
    out[i] = max(temp);
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_abind_check_conform_dims)]]
LogicalVector rcpp_abind_check_conform_dims(
  IntegerVector conform_dim, IntegerMatrix arg_dim, int n, int along
) {
  LogicalVector out(n);
  int m = arg_dim.nrow();
  for(int i = 0; i < n; ++i) {
    int counter = 0;
    for(int j = 0; j < m; ++j) {
      if(j != (along - 1)) {
        if(conform_dim[j] != arg_dim(j, i)) {
          counter++;
        }
      }
    }
    if(counter == 0) {
      out[i] = true;
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_abind_get_dimnames)]]
List rcpp_abind_get_dimnames(
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rcbind_get_sizes)]]
IntegerVector rcpp_rcbind_get_sizes(
    List lst, int imargin
  ) {
    int n = lst.length();
    IntegerVector out(n);
    for(int i = 0; i < n; ++i) {
      RObject temp = lst[i];
      out[i] = Rf_length(temp);
      if(temp.hasAttribute("dim")) {
        IntegerVector dims = temp.attr("dim");
        if(Rf_length(dims) >= (imargin + 1)) {
          out[i] = dims[imargin];
        }
      }
    }
    return out;
  }

