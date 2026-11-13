#include <Rcpp/Lightest>

using namespace Rcpp;




#define MACRO_TERNARY_ASSIGN_C(INPUTCODE) do {  \
pout[i] = INPUTCODE;                            \
} while(0)


#define MACRO_TERNARY_ASSIGN_STRING(INPUTCODE) do {  \
SET_STRING_ELT(out, i, INPUTCODE);                   \
} while(0)


#define MACRO_TERNARY_ASSIGN_LIST(INPUTCODE) do {  \
SET_VECTOR_ELT(out, i, INPUTCODE);                 \
} while(0)


#define MACRO_TERNARY_OP_OUT(ASSIGNCODE) do { \
R_xlen_t n = Rf_xlength(x);                   \
for(R_xlen_t i = 0; i < n; ++i) {             \
  if(px[i] == 1) {                            \
    ASSIGNCODE(truevalue);                    \
  }                                           \
  else if(px[i] == 0) {                       \
    ASSIGNCODE(falsevalue);                   \
  }                                           \
  else {                                      \
    ASSIGNCODE(navalue);                      \
  }                                           \
}                                             \
} while(0)


#define MACRO_TERNARY_OUT do {                       \
switch(TYPEOF(out)) {	                               \
case LGLSXP:	                                        \
{	                                                   \
  int *pout = LOGICAL(out);	                         \
  const int truevalue = LOGICAL_RO(values)[0];       \
  const int falsevalue = LOGICAL_RO(values)[1];      \
  const int navalue = LOGICAL_RO(values)[2];         \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_C);      \
                                                     \
  break;                                             \
}	                                                   \
case INTSXP:	                                        \
{	                                                   \
  int *pout = LOGICAL(out);	                         \
  const int truevalue = INTEGER_RO(values)[0];       \
  const int falsevalue = INTEGER_RO(values)[1];      \
  const int navalue = INTEGER_RO(values)[2];         \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_C);      \
                                                     \
  break;                                             \
}	                                                   \
case REALSXP:	                                       \
{	                                                   \
  double *pout = REAL(out);	                         \
  const double truevalue = REAL_RO(values)[0];       \
  const double falsevalue = REAL_RO(values)[1];      \
  const double navalue = REAL_RO(values)[2];         \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_C);      \
                                                     \
  break;                                             \
}	                                                   \
case CPLXSXP:	                                       \
{	                                                   \
  Rcomplex *pout = COMPLEX(out);	                    \
  const Rcomplex truevalue = COMPLEX_RO(values)[0];  \
  const Rcomplex falsevalue = COMPLEX_RO(values)[1]; \
  const Rcomplex navalue = COMPLEX_RO(values)[2];    \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_C);      \
                                                     \
  break;                                             \
}	                                                   \
case STRSXP:	                                        \
{	                                                   \
  const SEXP truevalue = STRING_PTR_RO(values)[0];   \
  const SEXP falsevalue = STRING_PTR_RO(values)[1];  \
  const SEXP navalue = STRING_PTR_RO(values)[2];     \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_STRING); \
                                                     \
  break;                                             \
}	                                                   \
case RAWSXP:	                                        \
{	                                                   \
  Rbyte *pout = RAW(out);	                           \
  const Rbyte truevalue = RAW_RO(values)[0];         \
  const Rbyte falsevalue = RAW_RO(values)[1];        \
  const Rbyte navalue = RAW_RO(values)[2];           \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_C);      \
                                                     \
  break;                                             \
}	                                                   \
case VECSXP:	                                        \
{	                                                   \
  const SEXP truevalue = VECTOR_ELT(values, 0);      \
  const SEXP falsevalue = VECTOR_ELT(values, 1);     \
  const SEXP navalue = VECTOR_ELT(values, 2);        \
                                                     \
  MACRO_TERNARY_OP_OUT(MACRO_TERNARY_ASSIGN_LIST);   \
                                                     \
  break;                                             \
}	                                                   \
default:	                                            \
{	                                                   \
  stop("unsupported type");	                         \
}	                                                   \
}	                                                   \
} while(0)






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ternary, rng = false)]]
 void rcpp_ternary(
     SEXP out, SEXP x, SEXP values
 ) {
   
   const Rbyte *px = RAW_RO(x);
   
   MACRO_TERNARY_OUT;
 }
