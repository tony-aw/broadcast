#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_lst_ndims)]]
SEXP rcpp_lst_ndims ( SEXP x ) {


if(TYPEOF(x) != VECSXP) {
  stop("`x` must be a list");
}

int n = Rf_length(x);
RObject tempout;
int tempndims;

IntegerVector out(no_init(n));
int *pout = INTEGER(out);

for(int i = 0; i < n; ++i) {
  tempout = VECTOR_ELT(x, i);
  tempndims = Rf_length(tempout.attr("dim"));
  pout[i] = tempndims;
}

return out;

}
