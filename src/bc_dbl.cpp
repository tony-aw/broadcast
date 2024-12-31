

#include <Rcpp.h>

using namespace Rcpp;




#define MACRO_ACTION_COMMON(NACHECK, NACODE, DOCODE) do {      \
  if(NACHECK) {                                                   \
  	  NACODE;                                                       \
  	}                                                               \
  	else {                                                          \
  	  DOCODE;                                                       \
  	}                                                               \
} while(0)




#define MACRO_ACTION_SPECIAL(RULECHECK, RULECODE, NACHECK, NACODE, DOCODE) do {      \
  if(RULECHECK) {                                                   \
    RULECODE;                                                       \
  }                                                                 \
  else if(NACHECK) {                                                     \
  	  NACODE;                                                       \
  }                                                               \
	else {                                                          \
	  DOCODE;                                                       \
	}                                                               \
} while(0)




#define MACRO_DIM_VECTOR(DOCODE) do {                               \
  R_xlen_t flatind_x = 0;                                           \
  R_xlen_t flatind_y = 0;                                           \
  int by_x = 0;                                                     \
  int by_y = 0;                                                     \
  if(Rf_xlength(x) == Rf_xlength(y)) {                              \
    if(Rf_xlength(x) == 1) {                                        \
      by_x = 0;                                                     \
      by_y = 0;                                                     \
    }                                                               \
    else {                                                          \
      by_x = 1;                                                     \
      by_y = 1;                                                     \
    }                                                               \
  }                                                                 \
  if(Rf_xlength(x) != Rf_xlength(y)) {                              \
    if(Rf_xlength(x) == 1) {                                        \
      by_x = 0;                                                     \
      by_y = 1;                                                     \
    }                                                               \
    else if(Rf_xlength(y) ==1) {                                    \
      by_x = 1;                                                     \
      by_y = 0;                                                     \
    }                                                               \
    else {                                                          \
      stop("unequal length");                                     \
    }                                                               \
  }                                                                 \
  for(R_xlen_t i = 0; i < nout; ++i) {                              \
      DOCODE;                                                       \
  	                                                                \
  	  pout[i] = tempout;                                            \
      flatind_x = flatind_x + by_x;                                 \
      flatind_y = flatind_y + by_y;                                 \
                                                                    \
    }                                                               \
                                                                    \
} while(0)




#define MACRO_DIM_2(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
} while(0)




#define MACRO_DIM_3(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_4(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_5(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter5 = 0; iter5 < pout_dim[4]; ++iter5) {	\
	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]) + pdcp_x[4] * (iter5 * pby_x[4]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]) + pdcp_y[4] * (iter5 * pby_y[4]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_6(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter6 = 0; iter6 < pout_dim[5]; ++iter6) {	\
	 for(int iter5 = 0; iter5 < pout_dim[4]; ++iter5) {	\
	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]) + pdcp_x[4] * (iter5 * pby_x[4]) + pdcp_x[5] * (iter6 * pby_x[5]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]) + pdcp_y[4] * (iter5 * pby_y[4]) + pdcp_y[5] * (iter6 * pby_y[5]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_7(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter7 = 0; iter7 < pout_dim[6]; ++iter7) {	\
	 for(int iter6 = 0; iter6 < pout_dim[5]; ++iter6) {	\
	 for(int iter5 = 0; iter5 < pout_dim[4]; ++iter5) {	\
	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]) + pdcp_x[4] * (iter5 * pby_x[4]) + pdcp_x[5] * (iter6 * pby_x[5]) + pdcp_x[6] * (iter7 * pby_x[6]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]) + pdcp_y[4] * (iter5 * pby_y[4]) + pdcp_y[5] * (iter6 * pby_y[5]) + pdcp_y[6] * (iter7 * pby_y[6]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_8(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter8 = 0; iter8 < pout_dim[7]; ++iter8) {	\
	 for(int iter7 = 0; iter7 < pout_dim[6]; ++iter7) {	\
	 for(int iter6 = 0; iter6 < pout_dim[5]; ++iter6) {	\
	 for(int iter5 = 0; iter5 < pout_dim[4]; ++iter5) {	\
	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]) + pdcp_x[4] * (iter5 * pby_x[4]) + pdcp_x[5] * (iter6 * pby_x[5]) + pdcp_x[6] * (iter7 * pby_x[6]) + pdcp_x[7] * (iter8 * pby_x[7]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]) + pdcp_y[4] * (iter5 * pby_y[4]) + pdcp_y[5] * (iter6 * pby_y[5]) + pdcp_y[6] * (iter7 * pby_y[6]) + pdcp_y[7] * (iter8 * pby_y[7]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_16(DOCODE) do {      \
  R_xlen_t counter = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int *pout_dim = INTEGER_RO(out_dim);      \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  	 for(int iter16 = 0; iter16 < pout_dim[15]; ++iter16) {	\
	 for(int iter15 = 0; iter15 < pout_dim[14]; ++iter15) {	\
	 for(int iter14 = 0; iter14 < pout_dim[13]; ++iter14) {	\
	 for(int iter13 = 0; iter13 < pout_dim[12]; ++iter13) {	\
	 for(int iter12 = 0; iter12 < pout_dim[11]; ++iter12) {	\
	 for(int iter11 = 0; iter11 < pout_dim[10]; ++iter11) {	\
	 for(int iter10 = 0; iter10 < pout_dim[9]; ++iter10) {	\
	 for(int iter9 = 0; iter9 < pout_dim[8]; ++iter9) {	\
	 for(int iter8 = 0; iter8 < pout_dim[7]; ++iter8) {	\
	 for(int iter7 = 0; iter7 < pout_dim[6]; ++iter7) {	\
	 for(int iter6 = 0; iter6 < pout_dim[5]; ++iter6) {	\
	 for(int iter5 = 0; iter5 < pout_dim[4]; ++iter5) {	\
	 for(int iter4 = 0; iter4 < pout_dim[3]; ++iter4) {	\
	 for(int iter3 = 0; iter3 < pout_dim[2]; ++iter3) {	\
	 for(int iter2 = 0; iter2 < pout_dim[1]; ++iter2) {	\
	 for(int iter1 = 0; iter1 < pout_dim[0]; ++iter1) {	\
        flatind_x = iter1 * pby_x[0] + pdcp_x[1] * (iter2 * pby_x[1]) + pdcp_x[2] * (iter3 * pby_x[2]) + pdcp_x[3] * (iter4 * pby_x[3]) + pdcp_x[4] * (iter5 * pby_x[4]) + pdcp_x[5] * (iter6 * pby_x[5]) + pdcp_x[6] * (iter7 * pby_x[6]) + pdcp_x[7] * (iter8 * pby_x[7]) + pdcp_x[8] * (iter9 * pby_x[8]) + pdcp_x[9] * (iter10 * pby_x[9]) + pdcp_x[10] * (iter11 * pby_x[10]) + pdcp_x[11] * (iter12 * pby_x[11]) + pdcp_x[12] * (iter13 * pby_x[12]) + pdcp_x[13] * (iter14 * pby_x[13]) + pdcp_x[14] * (iter15 * pby_x[14]) + pdcp_x[15] * (iter16 * pby_x[15]);       \
        flatind_y = iter1 * pby_y[0] + pdcp_y[1] * (iter2 * pby_y[1]) + pdcp_y[2] * (iter3 * pby_y[2]) + pdcp_y[3] * (iter4 * pby_y[3]) + pdcp_y[4] * (iter5 * pby_y[4]) + pdcp_y[5] * (iter6 * pby_y[5]) + pdcp_y[6] * (iter7 * pby_y[6]) + pdcp_y[7] * (iter8 * pby_y[7]) + pdcp_y[8] * (iter9 * pby_y[8]) + pdcp_y[9] * (iter10 * pby_y[9]) + pdcp_y[10] * (iter11 * pby_y[10]) + pdcp_y[11] * (iter12 * pby_y[11]) + pdcp_y[12] * (iter13 * pby_y[12]) + pdcp_y[13] * (iter14 * pby_y[13]) + pdcp_y[14] * (iter15 * pby_y[14]) + pdcp_y[15] * (iter16 * pby_y[15]);     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        pout[counter] = tempout;        \
        counter++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_DOCALL(DOCODE) do {     \
  int ndims = Rf_length(out_dim);         \
                                          \
  switch(ndims) {       \
    case 2:                                       \
  MACRO_DIM_2(DOCODE);    \
  break;                                        \
case 3:                                       \
  MACRO_DIM_3(DOCODE);    \
  break;                                        \
case 4:                                       \
  MACRO_DIM_4(DOCODE);    \
  break;                                        \
case 5:                                       \
  MACRO_DIM_5(DOCODE);    \
  break;                                        \
case 6:                                       \
  MACRO_DIM_6(DOCODE);    \
  break;                                        \
case 7:                                       \
  MACRO_DIM_7(DOCODE);    \
  break;                                        \
case 8:                                       \
  MACRO_DIM_8(DOCODE);    \
  break;                                        \
       \
  }       \
} while(0)



#define MACRO_DIM_GENERAL(DOCODE) do {                              \
  const void *vmaxsave = vmaxget();                                 \
                                                                    \
  int k = Rf_length(xdims1);                                        \
  const int *pxdims1 = INTEGER_RO(xdims1);                          \
  const int *pxdims2 = INTEGER_RO(xdims2);                          \
                                                                    \
  int **subs1 = (int**)R_alloc(k, sizeof(int*));                    \
  int *indx1 = (int*)R_alloc(k, sizeof(int));                       \
  int *bound1 = (int*)R_alloc(k, sizeof(int));                      \
  R_xlen_t *offset1 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));      \
                                                                    \
  int **subs2 = (int**)R_alloc(k, sizeof(int*));                    \
  int *indx2 = (int*)R_alloc(k, sizeof(int));                       \
  int *bound2 = (int*)R_alloc(k, sizeof(int));                      \
  R_xlen_t *offset2 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));      \
                                                                    \
                                                                    \
  R_xlen_t n1 = 1;                                                  \
  SEXP r1;                                                          \
  R_xlen_t n2 = 1;                                                  \
  SEXP r2;                                                          \
                                                                    \
  for (int i = 0; i < k; i++) {                                     \
    r1 = VECTOR_ELT(s1, i);                                         \
  	indx1[i] = 0;                                                   \
  	bound1[i] = Rf_length(r1);                                      \
    n1 *= bound1[i];                                                \
  	subs1[i] = INTEGER(r1);                                         \
  	                                                                \
  	r2 = VECTOR_ELT(s2, i);                                         \
  	indx2[i] = 0;                                                   \
  	bound2[i] = Rf_length(r2);                                      \
    n2 *= bound2[i];                                                \
  	subs2[i] = INTEGER(r2);                                         \
  }                                                                 \
                                                                    \
  offset1[0] = 1;                                                   \
  offset2[0] = 1;                                                   \
  for (int i = 1; i < k; i++) {                                     \
    offset1[i] = offset1[i - 1] * pxdims1[i - 1];                   \
    offset2[i] = offset2[i - 1] * pxdims2[i - 1];                   \
  }                                                                 \
                                                                    \
                                                                    \
  R_xlen_t counter = 0;                                             \
  R_xlen_t flatind_x;                                               \
  R_xlen_t flatind_y;                                               \
                                                                    \
  for (R_xlen_t i = 0; i < nout; i++) {                             \
                                                                    \
    flatind_x = 0;                                                  \
    flatind_y = 0;                                                  \
                                                                    \
  	for (int j = 0; j < k; j++) {                                   \
	    int jj1 = subs1[j][indx1[j]];                                 \
	    flatind_x += (jj1 - 1) * offset1[j];                          \
	    int jj2 = subs2[j][indx2[j]];                                 \
	    flatind_y += (jj2 - 1) * offset2[j];                          \
  	}                                                               \
  	                                                                \
  	                                                                \
  	DOCODE;                                                         \
  	                                                                \
  	pout[counter] = tempout;                                        \
    counter++;                                                      \
  	                                                                \
	  int j1 = 0;                                                     \
    while (++indx1[j1] >= bound1[j1]) {                             \
  		indx1[j1] = 0;                                                \
  		j1 = (j1 + 1) % k;                                            \
    }                                                               \
  	int j2 = 0;                                                     \
    while (++indx2[j2] >= bound2[j2]) {                             \
  		indx2[j2] = 0;                                                \
  		j2 = (j2 + 1) % k;                                            \
    }                                                               \
	}                                                                 \
                                                                    \
                                                                    \
  vmaxset(vmaxsave);                                                \
                                                                    \
} while(0)




#define MACRO_TYPESWITCH_NUMERIC_COMMON(DIMCODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_COMMON(                                           \
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(xint && !yint) {                                  \
    const int *px = INTEGER_RO(x);                                        \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_COMMON(                                           \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_COMMON(                                           \
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_COMMON(                                           \
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_NUMERIC_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_SPECIAL(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(xint && !yint) {                                  \
    const int *px = INTEGER_RO(x);                                        \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_SPECIAL(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_SPECIAL(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_SPECIAL(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_NUMERIC_SIMPLE(DIMCODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(DOCODE);                                                      \
  }                                                         \
  else if(xint && !yint) {                                  \
    const int *px = INTEGER_RO(x);                                        \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(DOCODE);                                                      \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(DOCODE);                                                      \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(DOCODE);                                                      \
  }                                                         \
} while(0)



  
  
  //' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_test)]]
int rcpp_test(int x, int y) {
  return (x + y);
}
  
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_v)]]
SEXP rcpp_bc_dbl_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_VECTOR,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_d)]]
SEXP rcpp_bc_dbl_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_general)]]
SEXP rcpp_bc_dbl_general(
  SEXP x, SEXP y,
  const SEXP s1, const SEXP s2,
  const SEXP xdims1, const SEXP xdims2, const R_xlen_t nout, int op
) {
  
  double tempout;

  SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
  double *pout;
  pout = REAL(out);
  
  switch(op) {
    case 1:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] + (double)py[flatind_y]
      );
      break;
    }
    case 2:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] - (double)py[flatind_y]
      );
      break;
    }
    case 3:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] * (double)py[flatind_y]
      );
      break;
    }
    case 4:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] / (double)py[flatind_y]
      );
      break;
    }
    case 5:
    {
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(
        MACRO_DIM_GENERAL,
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
        tempout = 1,
        tempout = NA_REAL,
        tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
      );
      break;
    }
    case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
    default:
    {
      stop("given operator not supported in the given context");
    }
  
  }
  
  
  UNPROTECT(1);
  return out;
  
  
}


