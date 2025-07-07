#include <Rcpp/Lightest>
using namespace Rcpp;



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_allocate_nestedlist)]]
List rcpp_allocate_nestedlist(
  NumericVector lens, int depth
) {

  int n = lens[depth - 1];
  List out(n);
  
  if(depth == Rf_length(lens)) {
    return out;
  }
  
  for(int i = 0; i < n; ++i) {
      out[i] = rcpp_allocate_nestedlist(lens, depth + 1);
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rec_dim2hier)]]
void rcpp_rec_dim2hier(
  SEXP x, SEXP out, SEXP dcp, int dimpart_prev, int depth, int depth_target
) {
  const double dcp_current = REAL_RO(dcp)[depth - 1];
  R_xlen_t dimpart_current;
  
  if(depth == depth_target) {
    for(R_xlen_t i = 0; i < Rf_xlength(out); ++i) {
      dimpart_current = i * dcp_current + dimpart_prev;
      SEXP temp = VECTOR_ELT(x, dimpart_current);
      SET_VECTOR_ELT(out, i, temp);
    }
  }
  else {
    for(R_xlen_t i = 0; i < Rf_xlength(out); ++i) {
      dimpart_current = i * dcp_current + dimpart_prev;
      SEXP temp = VECTOR_ELT(out, i);
      rcpp_rec_dim2hier(x, temp, dcp, dimpart_current, depth + 1, depth_target);
    }
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_rec_dim2hier_names)]]
void rcpp_rec_dim2hier_names(
  List out, List dimnames, int depth, int depth_target
) {
  
  if(dimnames[depth] != R_NilValue) {
    CharacterVector dimnames_current = dimnames[depth];
    for(R_xlen_t i = 0; i < Rf_xlength(out); ++i) {
      List temp = VECTOR_ELT(out, i);
      temp.attr("names") = dimnames_current;
    }
  }
  
  if(depth < depth_target) {
    for(R_xlen_t i = 0; i < Rf_xlength(out); ++i) {
      List temp = VECTOR_ELT(out, i);
      rcpp_rec_dim2hier_names(temp, dimnames, depth + 1, depth_target);
    }
  }
}

