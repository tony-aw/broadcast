

#include <Rcpp/Lightest>
using namespace Rcpp;




// 2 ADJACENT dimensions i and i+1 of arrays x and y can be merged if,
// and only if, ALL of the following is TRUE:
//  -> dim(x)[i] and dim(x)[i + 1] are not auto-orthogonal AND dim(y)[i] and dim(y)[i + 1] are not auto-orthogonal
//  -> (dim(x)[i] * dim(x)[i + 1]) < (2^31-1).
//  -> (dim(y)[i] * dim(y)[i + 1]) < (2^31-1).
// i.e. if x.dim[1:2] = c(1, 1) and y.dim[1:2] = c(2, 3),
// x.dim[1:2] can be merged to become 1 and y.dim[1:2] to become 6 (= prod(c(2, 3))).
// But if x.dim[1:3] = c(1, 9, 1) and y.dim = c(8, 1, 8),
// x.dim[1:3] is auto-orthogonal, and so is y.dim[1:3], and thus they CANNOT be merged.
// Merging prevents unnecessary broadcasting,
// which in turn makes the actual broadcasting more efficient.

// Note that the following is probably not the most efficient code.
// The code was written primarily to be readible.
// The power of C++ is used to make it less slow than 'R'.




//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims_get_endrange)]]
 int rcpp_mergedims_get_endrange(SEXP x, SEXP y, int pos, double intmax) {
   
   int *px = INTEGER(x);
   int *py = INTEGER(y);
   int n = Rf_length(x);
   
   bool merge_x, merge_y, drop_next;
   double prod_x = (double)px[pos];
   double prod_y = (double)py[pos];
   
   
   if(pos == (n - 1)) { // if `pos` is last position, exit and return pos;
     return pos;
   }
   
   // else, start at next position:
   int i;
   for(i = (pos + 1); i < n; ++i) {
     merge_x = (px[pos] == 1) == (px[i] == 1);
     merge_y = (py[pos] == 1) == (py[i] == 1);
     drop_next = (px[i] == 1) && (py[i] == 1);
     if((merge_x && merge_y) || drop_next) {
       prod_x *= (double)px[i];
       prod_y *= (double)py[i];
       if((prod_x >= intmax) || (prod_y >= intmax)) {
         return (i - 1);
       }
     }
     else {
       return (i - 1);
     }
   }
   
   return i -1;
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims_get_prods)]]
 Rcomplex rcpp_mergedims_get_prods(SEXP x, SEXP y, int start, int end) {
   
   int *px = INTEGER(x);
   int *py = INTEGER(y);
   
   double prod_x = (double)px[start];
   double prod_y = (double)py[start];
   Rcomplex out;
   
   
   // if start = i and end = i, this if() statement is run:
   if(end == start) {
     out.r = prod_x;
     out.i = prod_y;
     return out;
   }
   
   // start+1 because we don't want to multiply x[i] with itself;
   // i <= end instead of i < end, because (unlike `n`) `end` is always smaller than length(x);
   for(int i = (start + 1); i <= end; ++i) {
     prod_x *= px[i];
     prod_y *= py[i];
   }
   
   out.r = prod_x;
   out.i = prod_y;
   return out;
   
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims)]]
 SEXP rcpp_mergedims(SEXP x, SEXP y) {
   
   int n = Rf_length(x);
   int *bufx = (int *) R_alloc(n, sizeof(int));
   int *bufy = (int *) R_alloc(n, sizeof(int));
   SEXP outx;
   SEXP outy;
   
   int start = 0;
   int end = 0;
   Rcomplex prods;
   double intmax = pow(2, 31) - 1;
   
   int i;
   
   for(i = 0; i < n; ++i) {
     end = rcpp_mergedims_get_endrange(x, y, start, intmax);
     prods = rcpp_mergedims_get_prods(x, y, start, end);
     bufx[i] = (int) (prods.r);
     bufy[i] = (int) (prods.i);
     start = end + 1;
     
     if(end >= (n - 1)) {
       // n - 1 because cpp starts counting at 0 (obviously);
       break;
     }
     
   }
   
   int len = i + 1; // again, cpp starts counting at zero, but we want the length
   
   PROTECT(outx = Rf_allocVector(INTSXP, len));
   PROTECT(outy = Rf_allocVector(INTSXP, len));
   if(len) {
     memcpy(INTEGER(outx), bufx, sizeof(int) * len);
     memcpy(INTEGER(outy), bufy, sizeof(int) * len);
     
   }
   
   SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
   SET_VECTOR_ELT(out, 0, outx);
   SET_VECTOR_ELT(out, 1, outy);
   
   UNPROTECT(3);
   
   return out;
   
 }