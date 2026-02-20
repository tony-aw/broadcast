 
#include <Rcpp/Lightest>
using namespace Rcpp;




#define MACRO_ECP_DETERMINE_BROADCASTING do {       \
  nobs = nrow > Rf_xlength(y) ? nrow : Rf_xlength(y); \
  if(Rf_xlength(y) == nobs) {                         \
    by_y = 1;                                         \
  }                                                   \
  else if(Rf_xlength(y) == 1) {                       \
    by_y = 0;                                         \
  }                                                   \
  else {                                              \
    stop("not comformable for broadcasting");         \
  }                                                   \
  if(nrow == nobs) {                                  \
    by_sim = 1;                                       \
  }                                                   \
  else if(nrow == 1) {                                \
    by_sim = 0;                                       \
  }                                                   \
  else {                                              \
    stop("not comformable for broadcasting");       \
  }                                                   \
} while(0);



#define MACRO_ECP_DO(NAFUN, TEMP_SIM) do {  \
  for(int i = 0; i < nobs; ++i) { \
     temp_sum = 0;  \
     temp_NA = 0; \
     if(NAFUN(py[i * by_y])) {  \
       temp_NA = 1; \
     }  \
     else { \
       for(int j = 0; j < ncol; ++j) {  \
         temp_sim = TEMP_SIM;  \
         if(NAFUN(temp_sim)) {  \
           temp_NA = 1; \
           break; \
         }  \
         temp_sum += (temp_sim <= py[i * by_y]);  \
       }  \
     }  \
      if(temp_NA) {                             \
        pout[i] = NA_REAL;                      \
      }                                         \
      else {                                    \
        temp = (double)temp_sum / (double)ncol; \
        if(temp < cutmin) {                     \
          pout[i] = cutmin;                     \
        }                                       \
        else if(temp > cutmax)  {               \
          pout[i] = cutmax;                     \
        }                                       \
        else {                                  \
          pout[i] = temp;                       \
        }                                       \
      }                                         \
   }  \
} while(0);




inline bool rcpp_isna_int(int x) {
  return x == NA_INTEGER;
}

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_ecp_mat)]]
 SEXP rcpp_ecp_mat(
     SEXP y, SEXP sim, int nrow, int ncol, double eps
 ) {
   
   // determine vectorization and broadcasting:
   int nobs;
   int by_y, by_sim;
   MACRO_ECP_DETERMINE_BROADCASTING;
   
   // declare common variables:
   const double cutmin = eps;
   const double cutmax = 1 - eps;
   double temp;
   int temp_sum;
   int temp_NA;
   
   
   // allocate output:
   SEXP out = PROTECT(Rf_allocVector(REALSXP, nobs));
   double *pout = REAL(out);
   
   
   if(TYPEOF(y) == REALSXP) {
     const double *psim = REAL_RO(sim);
     const double *py = REAL_RO(y);
     double temp_sim;
     
     MACRO_ECP_DO(R_isnancpp, psim[i * by_sim + nrow * j]);
   }
   else if(TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP){
     const int *psim = INTEGER_RO(sim);
     const int *py = INTEGER_RO(y);
     int temp_sim;
     
     MACRO_ECP_DO(rcpp_isna_int, psim[i * by_sim + nrow * j]);
   }
   else {
     stop("unsupported types given");
   }
   
   
   UNPROTECT(1);
   return out;
 }

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_ecp_df)]]
 SEXP rcpp_ecp_df(
     SEXP y, SEXP sim, int nrow, int ncol, double eps
 ) {
   
   // determine vectorization and broadcasting:
   int nobs;
   int by_y, by_sim;
   MACRO_ECP_DETERMINE_BROADCASTING;
   
   // declare common variables:
   const double cutmin = eps;
   const double cutmax = 1 - eps;
   double temp;
   int temp_sum;
   int temp_NA;
   SEXP temp_pointer;
   
   // allocate output:
   SEXP out = PROTECT(Rf_allocVector(REALSXP, nobs));
   double *pout = REAL(out);
   
   
   if(TYPEOF(y) == INTSXP || TYPEOF(y) == LGLSXP) {
     
     // Create array of pointers:
     std::vector<const int*> psim(ncol);
     for(int i = 0; i < ncol; ++i) {
       temp_pointer = VECTOR_ELT(sim, i);
       if(!(TYPEOF(temp_pointer) == INTSXP || TYPEOF(temp_pointer) == LGLSXP)) {
         stop("all columns of `sim` must be of the same type");
       }
       psim[i] = INTEGER_RO(VECTOR_ELT(sim, i));
     }
     
     const int *py = INTEGER_RO(y);
     int temp_sim;
     
     MACRO_ECP_DO(rcpp_isna_int, psim[j][i * by_sim]);
     
     UNPROTECT(1);
     return out;
   }
   
   else if(TYPEOF(y) == REALSXP) {
     
     // Create array of pointers:
     std::vector<const double*> psim(ncol);
     for(int i = 0; i < ncol; ++i) {
       temp_pointer = VECTOR_ELT(sim, i);
       if(TYPEOF(temp_pointer) != REALSXP) {
         stop("all columns of `sim` must be of the same type");
       }
       psim[i] = REAL_RO(VECTOR_ELT(sim, i));
     }
     
     const double *py = REAL_RO(y);
     double temp_sim;
     
     MACRO_ECP_DO(R_isnancpp, psim[j][i * by_sim]);
     
     UNPROTECT(1);
     return out;
     
   }
   else {
     stop("unsupported types given");
   }
   
 }


