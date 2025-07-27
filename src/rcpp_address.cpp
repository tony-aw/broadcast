

#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_address)]]
String rcpp_address(SEXP x) {
  // based on 'R'-source `do_tracemem()`, but doing the 'C++' equivalent, and only getting the address (no explicit tracing involved):
  char buffer[20];
  std::snprintf(buffer, 20, "<%p>", (void *) x);
  std::string buffer2 = buffer;
  return buffer2;
  
}
