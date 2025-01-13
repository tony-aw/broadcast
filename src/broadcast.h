

#ifndef BROADCAST_H
#define BROADCAST_H



#define MACRO_ACTION1(DOCODE) do {      \
  DOCODE;                                     \
} while(0)




#define MACRO_ACTION2(NACHECK, NACODE, DOCODE) do {      \
  if(NACHECK) {                                                   \
  	  NACODE;                                                       \
  	}                                                               \
  	else {                                                          \
  	  DOCODE;                                                       \
  	}                                                               \
} while(0)




#define MACRO_ACTION3(RULECHECK, RULECODE, DOCODE) do {      \
  if(RULECHECK) {                                                   \
    RULECODE;                                                       \
  }                                                                 \
	else {                                                          \
	  DOCODE;                                                       \
	}                                                               \
} while(0)




#define MACRO_ACTION4(RULECHECK, RULECODE, NACHECK, NACODE, DOCODE) do {      \
  if(RULECHECK) {                                                   \
    RULECODE;                                                       \
  }                                                                 \
  else if(NACHECK) {                                                \
  	  NACODE;                                                       \
  }                                                               \
	else {                                                          \
	  DOCODE;                                                       \
	}                                                               \
} while(0)



#define MACRO_ACTION_BOOLEAN(XREF, YREF, PRECHECK, PRECODE, NACODE, DOCODE) do { \
                                          \
  xTRUE = rcpp_isTRUE(XREF);                 \
  xFALSE = rcpp_isFALSE(XREF);               \
  xNA = XREF == NA_INTEGER;                  \
  yTRUE = rcpp_isTRUE(YREF);                 \
  yFALSE = rcpp_isFALSE(YREF);               \
  yNA = YREF == NA_INTEGER;                  \
  if(PRECHECK) {                          \
    PRECODE;                              \
  }                                       \
  else if(xNA || yNA) {                   \
    NACODE;                               \
  }                                       \
  else {                                  \
    DOCODE;                               \
  }                                       \
} while(0)



#define MACRO_DOUBLEPASS(MACRO1, MACRO2) do{  \
  MACRO1;                                     \
  MACRO2;                                     \
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
  for(R_xlen_t flatind_out = 0; flatind_out < nout; ++flatind_out) {  \
      DOCODE;                                                       \
      flatind_x = flatind_x + by_x;                                 \
      flatind_y = flatind_y + by_y;                                 \
                                                                    \
    }                                                               \
                                                                    \
} while(0)




#define MACRO_DIM_ORTHOVECTOR(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int N1 = INTEGER(out_dim)[0];      \
  const int N2 = INTEGER(out_dim)[1];       \
  if(RxC) { \
    for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {	\
  	  for(int flatind_x = 0; flatind_x < N1; ++flatind_x) {	\
        DOCODE;                         \
        flatind_out++;                      \
    	 }	\
  	 }	\
  } \
  else {  \
    for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {	\
    	  for(int flatind_y = 0; flatind_y < N1; ++flatind_y) {	\
          DOCODE;                         \
          flatind_out++;                      \
        }	\
    }	\
  } \
} while(0)




#define MACRO_DIM_BIGX_2(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2; \
  	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGX_4(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4; \
  	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGX_6(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6; \
  	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGX_8(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8; \
  	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGX_10(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10; \
  	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGX_12(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12; \
  	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGX_14(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12, i_y13, i_y14; \
  	 for(int iter14 = 0; iter14 < N14; ++iter14) {	\
i_y14 = pby_y[13] * iter14 * pdcp_y[13];	\
	 for(int iter13 = 0; iter13 < N13; ++iter13) {	\
i_y13 = pby_y[12] * iter13 * pdcp_y[12];	\
	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12 + i_y13 + i_y14;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGX_16(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_y = INTEGER_RO(by_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
const int N15 = INTEGER(out_dim)[14];	\
const int N16 = INTEGER(out_dim)[15];	\
  const double *pdcp_y = REAL_RO(dcp_y);        \
  R_xlen_t flatind_x = 0;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12, i_y13, i_y14, i_y15, i_y16; \
  	 for(int iter16 = 0; iter16 < N16; ++iter16) {	\
i_y16 = pby_y[15] * iter16 * pdcp_y[15];	\
	 for(int iter15 = 0; iter15 < N15; ++iter15) {	\
i_y15 = pby_y[14] * iter15 * pdcp_y[14];	\
	 for(int iter14 = 0; iter14 < N14; ++iter14) {	\
i_y14 = pby_y[13] * iter14 * pdcp_y[13];	\
	 for(int iter13 = 0; iter13 < N13; ++iter13) {	\
i_y13 = pby_y[12] * iter13 * pdcp_y[12];	\
	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_y = iter1 * pby_y[0] + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12 + i_y13 + i_y14 + i_y15 + i_y16;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_x++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGY_2(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2; \
  	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGY_4(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4; \
  	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGY_6(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6; \
  	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGY_8(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8; \
  	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_BIGY_10(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10; \
  	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGY_12(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12; \
  	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGY_14(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14; \
  	 for(int iter14 = 0; iter14 < N14; ++iter14) {	\
i_x14 = pby_x[13] * iter14 * pdcp_x[13];	\
	 for(int iter13 = 0; iter13 < N13; ++iter13) {	\
i_x13 = pby_x[12] * iter13 * pdcp_x[12];	\
	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGY_16(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
const int N15 = INTEGER(out_dim)[14];	\
const int N16 = INTEGER(out_dim)[15];	\
  const double *pdcp_x = REAL_RO(dcp_x);        \
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y = 0;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14, i_x15, i_x16; \
  	 for(int iter16 = 0; iter16 < N16; ++iter16) {	\
i_x16 = pby_x[15] * iter16 * pdcp_x[15];	\
	 for(int iter15 = 0; iter15 < N15; ++iter15) {	\
i_x15 = pby_x[14] * iter15 * pdcp_x[14];	\
	 for(int iter14 = 0; iter14 < N14; ++iter14) {	\
i_x14 = pby_x[13] * iter14 * pdcp_x[13];	\
	 for(int iter13 = 0; iter13 < N13; ++iter13) {	\
i_x13 = pby_x[12] * iter13 * pdcp_x[12];	\
	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
	\
        flatind_x = iter1 * pby_x[0] + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14 + i_x15 + i_x16;     \
                                  \
        DOCODE;                   \
  	                              \
        flatind_y++;                    \
        flatind_out++;                      \
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




#define MACRO_DIM_BIGSMALL_DOCALL(DOCODE) do {     \
  int ndims = Rf_length(out_dim);         \
                                          \
  if(bigx) {                           \
    switch(ndims) {                       \
      case 2:                                       \
  MACRO_DIM_BIGX_2(DOCODE);    \
  break;                                        \
case 4:                                       \
  MACRO_DIM_BIGX_4(DOCODE);    \
  break;                                        \
case 6:                                       \
  MACRO_DIM_BIGX_6(DOCODE);    \
  break;                                        \
case 8:                                       \
  MACRO_DIM_BIGX_8(DOCODE);    \
  break;                                        \
case 10:                                       \
  MACRO_DIM_BIGX_10(DOCODE);    \
  break;                                        \
case 12:                                       \
  MACRO_DIM_BIGX_12(DOCODE);    \
  break;                                        \
case 14:                                       \
  MACRO_DIM_BIGX_14(DOCODE);    \
  break;                                        \
case 16:                                       \
  MACRO_DIM_BIGX_16(DOCODE);    \
  break;                                        \
                     \
    }                                     \
  }                                       \
  else {                                  \
    switch(ndims) {                       \
      case 2:                                       \
  MACRO_DIM_BIGY_2(DOCODE);    \
  break;                                        \
case 4:                                       \
  MACRO_DIM_BIGY_4(DOCODE);    \
  break;                                        \
case 6:                                       \
  MACRO_DIM_BIGY_6(DOCODE);    \
  break;                                        \
case 8:                                       \
  MACRO_DIM_BIGY_8(DOCODE);    \
  break;                                        \
case 10:                                       \
  MACRO_DIM_BIGY_10(DOCODE);    \
  break;                                        \
case 12:                                       \
  MACRO_DIM_BIGY_12(DOCODE);    \
  break;                                        \
case 14:                                       \
  MACRO_DIM_BIGY_14(DOCODE);    \
  break;                                        \
case 16:                                       \
  MACRO_DIM_BIGY_16(DOCODE);    \
  break;                                        \
                     \
    }                                     \
  }                                       \
} while(0)


#define MACRO_DIM_2(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2; \
  R_xlen_t i_y2; \
  	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2;       \
        flatind_y = pby_y[0] * iter1 + i_y2;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
  	 }	\
	 }	\
} while(0)




#define MACRO_DIM_4(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4; \
  R_xlen_t i_y2, i_y3, i_y4; \
  	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_6(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6; \
  	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_8(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8; \
  	 for(int iter8 = 0; iter8 <N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 <N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_10(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10; \
  	 for(int iter10 = 0; iter10 <N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 <N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 <N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 <N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
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




#define MACRO_DIM_12(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12; \
  	 for(int iter12 = 0; iter12 <N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 <N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 <N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 <N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 <N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 <N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
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




#define MACRO_DIM_14(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12, i_y13, i_y14; \
  	 for(int iter14 = 0; iter14 <N14; ++iter14) {	\
i_x14 = pby_x[13] * iter14 * pdcp_x[13];	\
i_y14 = pby_y[13] * iter14 * pdcp_y[13];	\
	 for(int iter13 = 0; iter13 <N13; ++iter13) {	\
i_x13 = pby_x[12] * iter13 * pdcp_x[12];	\
i_y13 = pby_y[12] * iter13 * pdcp_y[12];	\
	 for(int iter12 = 0; iter12 <N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 <N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 <N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 <N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 <N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 <N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12 + i_y13 + i_y14;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
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




#define MACRO_DIM_16(DOCODE) do {      \
  R_xlen_t flatind_out = 0;         \
  const int *pby_x = INTEGER_RO(by_x);        \
  const int *pby_y = INTEGER_RO(by_y);        \
  const double *pdcp_x = REAL_RO(dcp_x);        \
  const double *pdcp_y = REAL_RO(dcp_y);        \
  const int N1 = INTEGER(out_dim)[0];	\
const int N2 = INTEGER(out_dim)[1];	\
const int N3 = INTEGER(out_dim)[2];	\
const int N4 = INTEGER(out_dim)[3];	\
const int N5 = INTEGER(out_dim)[4];	\
const int N6 = INTEGER(out_dim)[5];	\
const int N7 = INTEGER(out_dim)[6];	\
const int N8 = INTEGER(out_dim)[7];	\
const int N9 = INTEGER(out_dim)[8];	\
const int N10 = INTEGER(out_dim)[9];	\
const int N11 = INTEGER(out_dim)[10];	\
const int N12 = INTEGER(out_dim)[11];	\
const int N13 = INTEGER(out_dim)[12];	\
const int N14 = INTEGER(out_dim)[13];	\
const int N15 = INTEGER(out_dim)[14];	\
const int N16 = INTEGER(out_dim)[15];	\
  R_xlen_t flatind_x;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14, i_x15, i_x16; \
  R_xlen_t i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12, i_y13, i_y14, i_y15, i_y16; \
  	 for(int iter16 = 0; iter16 <N16; ++iter16) {	\
i_x16 = pby_x[15] * iter16 * pdcp_x[15];	\
i_y16 = pby_y[15] * iter16 * pdcp_y[15];	\
	 for(int iter15 = 0; iter15 <N15; ++iter15) {	\
i_x15 = pby_x[14] * iter15 * pdcp_x[14];	\
i_y15 = pby_y[14] * iter15 * pdcp_y[14];	\
	 for(int iter14 = 0; iter14 <N14; ++iter14) {	\
i_x14 = pby_x[13] * iter14 * pdcp_x[13];	\
i_y14 = pby_y[13] * iter14 * pdcp_y[13];	\
	 for(int iter13 = 0; iter13 <N13; ++iter13) {	\
i_x13 = pby_x[12] * iter13 * pdcp_x[12];	\
i_y13 = pby_y[12] * iter13 * pdcp_y[12];	\
	 for(int iter12 = 0; iter12 <N12; ++iter12) {	\
i_x12 = pby_x[11] * iter12 * pdcp_x[11];	\
i_y12 = pby_y[11] * iter12 * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 <N11; ++iter11) {	\
i_x11 = pby_x[10] * iter11 * pdcp_x[10];	\
i_y11 = pby_y[10] * iter11 * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 <N10; ++iter10) {	\
i_x10 = pby_x[9] * iter10 * pdcp_x[9];	\
i_y10 = pby_y[9] * iter10 * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 <N9; ++iter9) {	\
i_x9 = pby_x[8] * iter9 * pdcp_x[8];	\
i_y9 = pby_y[8] * iter9 * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 <N8; ++iter8) {	\
i_x8 = pby_x[7] * iter8 * pdcp_x[7];	\
i_y8 = pby_y[7] * iter8 * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 <N7; ++iter7) {	\
i_x7 = pby_x[6] * iter7 * pdcp_x[6];	\
i_y7 = pby_y[6] * iter7 * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 <N6; ++iter6) {	\
i_x6 = pby_x[5] * iter6 * pdcp_x[5];	\
i_y6 = pby_y[5] * iter6 * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 <N5; ++iter5) {	\
i_x5 = pby_x[4] * iter5 * pdcp_x[4];	\
i_y5 = pby_y[4] * iter5 * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 <N4; ++iter4) {	\
i_x4 = pby_x[3] * iter4 * pdcp_x[3];	\
i_y4 = pby_y[3] * iter4 * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 <N3; ++iter3) {	\
i_x3 = pby_x[2] * iter3 * pdcp_x[2];	\
i_y3 = pby_y[2] * iter3 * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 <N2; ++iter2) {	\
i_x2 = pby_x[1] * iter2 * pdcp_x[1];	\
i_y2 = pby_y[1] * iter2 * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 <N1; ++iter1) {	\
	\
	\
        flatind_x = pby_x[0] * iter1 + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14 + i_x15 + i_x16;       \
        flatind_y = pby_y[0] * iter1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12 + i_y13 + i_y14 + i_y15 + i_y16;     \
                                                                    \
        DOCODE;                                                          \
  	                                                                \
        flatind_out++;                      \
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
case 4:                                       \
  MACRO_DIM_4(DOCODE);    \
  break;                                        \
case 6:                                       \
  MACRO_DIM_6(DOCODE);    \
  break;                                        \
case 8:                                       \
  MACRO_DIM_8(DOCODE);    \
  break;                                        \
case 10:                                       \
  MACRO_DIM_10(DOCODE);    \
  break;                                        \
case 12:                                       \
  MACRO_DIM_12(DOCODE);    \
  break;                                        \
case 14:                                       \
  MACRO_DIM_14(DOCODE);    \
  break;                                        \
case 16:                                       \
  MACRO_DIM_16(DOCODE);    \
  break;                                        \
       \
  }       \
} while(0)



#define MACRO_TYPESWITCH_NUMERIC_COMMON(DIMCODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION2(                                           \
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
      MACRO_ACTION2(                                           \
        px[flatind_x] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION2(                                           \
        py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION1(                                           \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_NUMERIC_CAREFUL(DIMCODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION2(                                           \
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
      MACRO_ACTION2(                                           \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION2(                                           \
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
      MACRO_ACTION2(                                           \
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




#define MACRO_TYPESWITCH_NUMERIC_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION4(                                           \
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
      MACRO_ACTION4(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        px[flatind_x] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION4(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION3(                                           \
        RULECHECK,                                                    \
        RULECODE,                                                     \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_NUMERIC_REL(DIMCODE, NACODE1, DOCODE1, NACODE2, DOCODE2) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                \
    const int *py = INTEGER_RO(y);                                \
    DIMCODE(                                                      \
      MACRO_DOUBLEPASS(                                           \
        MACRO_ACTION2(                                           \
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \
          NACODE1,                                               \
          DOCODE1                                                \
        ),                                                      \
        MACRO_ACTION2(                                          \
          R_isnancpp(tempcalc),                                   \
          NACODE2,                                               \
          DOCODE2                                               \
        )                                                       \
      )                                                         \
    );                                                          \
  }                                                             \
  else if(xint && !yint) {                                      \
    const int *px = INTEGER_RO(x);                              \
    const double *py = REAL_RO(y);                              \
    DIMCODE(                                                    \
      MACRO_DOUBLEPASS(                                         \
        MACRO_ACTION2(                                           \
          px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \
          NACODE1,                                               \
          DOCODE1                                                \
        ),                                                      \
        MACRO_ACTION2(                                          \
          R_isnancpp(tempcalc),                                   \
          NACODE2,                                               \
          DOCODE2                                               \
        )                                                       \
      )                                                         \
    );                                                          \
  }                                                             \
  else if(!xint && yint) {                                      \
    const double *px = REAL_RO(x);                              \
    const int *py = INTEGER_RO(y);                              \
    DIMCODE(                                                    \
      MACRO_DOUBLEPASS(                                         \
        MACRO_ACTION2(                                           \
          R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
          NACODE1,                                               \
          DOCODE1                                                \
        ),                                                      \
        MACRO_ACTION2(                                          \
          R_isnancpp(tempcalc),                                   \
          NACODE2,                                               \
          DOCODE2                                               \
        )                                                       \
      )                                                         \
    );                                                          \
  }                                                             \
  else if(!xint && !yint) {                                     \
    const double *px = REAL_RO(x);                              \
    const double *py = REAL_RO(y);                              \
    DIMCODE(                                                    \
      MACRO_DOUBLEPASS(                                         \
        MACRO_ACTION2(                                           \
          R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
          NACODE1,                                               \
          DOCODE1                                                \
        ),                                                      \
        MACRO_ACTION2(                                          \
          R_isnancpp(tempcalc),                                   \
          NACODE2,                                               \
          DOCODE2                                               \
        )                                                       \
      )                                                         \
    );                                                       \
  }                                                         \
} while(0)



#define MACRO_ASSIGN_C(INPUTCODE) do {  \
  tempout = INPUTCODE;              \
  pout[flatind_out] = tempout;      \
} while(0)


#define MACRO_OP_NUM_MATH(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] / (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(	\
        DIMCODE,	\
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,	\
        MACRO_ASSIGN_C(1),	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(R_pow((double)px[flatind_x], (double)py[flatind_y]))	\
      );	\
      break;	\
    }	\
    case 6:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(rcpp_mod_longint((double)px[flatind_x], (double)py[flatind_y])) 	\
      );	\
      break;	\
    }	\
    case 7:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\
      );	\
      break;	\
    }	\
    case 8:	\
    {	\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\
      );	\
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  }	\
} while(0)


#define MACRO_OP_NUM_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] == py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 2:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] != py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 3:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] < py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 4:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] > py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 5:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] <= py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 6:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] >= py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 7:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]), \
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc < prec)  \
    );	\
    break;	\
  }	\
  case 8:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]),	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc >= prec)  \
    );	\
    break;	\
  }	\
  case 9:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc <= -prec)  \
    );	\
    break;	\
  }	\
  case 10:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc >= prec)  \
    );	\
    break;	\
  }	\
  case 11:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc < prec)  \
    );	\
    break;	\
  }	\
  case 12:	\
  {	\
    MACRO_TYPESWITCH_NUMERIC_REL(	\
      DIMCODE,	\
      tempcalc = NA_REAL,	\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(tempcalc > -prec)  \
    );	\
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)


#define MACRO_OP_B_ANDOR(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      DIMCODE(                      \
        MACRO_ACTION_BOOLEAN(       \
          px[flatind_x], py[flatind_y],       \
          xFALSE || yFALSE,         \
          MACRO_ASSIGN_C(0),        \
          MACRO_ASSIGN_C(NA_LOGICAL),                                 \
          MACRO_ASSIGN_C((bool)px[flatind_x] && (bool)py[flatind_y])  \
        )                                                       \
      );                                                       \
      break;	\
    }	\
    case 2:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN(                                           \
          px[flatind_x], py[flatind_y],       \
          xTRUE || yTRUE,                   \
          MACRO_ASSIGN_C(1),                                            \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] || (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 3:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION2(                                                  \
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,   \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \
        )                                                       \
      );                                                                \
      break;	\
    }	\
    case 4:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN(                                           \
          px[flatind_x], py[flatind_y],       \
          xTRUE || yTRUE,                   \
          MACRO_ASSIGN_C(0),                                            \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C(!(bool)px[flatind_x] && !(bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  }	\
} while(0)


#define MACRO_OP_CPLX_MATH(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_plus(px[flatind_x], py[flatind_y])  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_min(px[flatind_x], py[flatind_y])  \
    );                                                                \
    break;	\
  }	\
  case 3:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_mult(px[flatind_x], py[flatind_y])  \
    );                                                                \
    break;	\
  }	\
  case 4:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_div(px[flatind_x], py[flatind_y])   \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)


#define MACRO_OP_CPLX_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_equal(px[flatind_x], py[flatind_y])  \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_cplx_unequal(px[flatind_x], py[flatind_y])  \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)


#define MACRO_OP_STR_CONC(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      out[flatind_out] = rcpp_string_plus(px[flatind_x], py[flatind_y]) \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)


#define MACRO_OP_STR_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      MACRO_ACTION2(                                                  \
        px[flatind_x] == NA_STRING || py[flatind_y] == NA_STRING,   \
        MACRO_ASSIGN_C(NA_LOGICAL),                                   \
        MACRO_ASSIGN_C((int)R_compute_identical(px[flatind_x], py[flatind_y], 0))  \
      )                                                       \
    );                                                                \
    break;	\
  }	\
  case 2:	\
  {	\
    DIMCODE(                                                          \
      MACRO_ACTION2(                                                  \
        px[flatind_x] == NA_STRING || py[flatind_y] == NA_STRING,   \
        MACRO_ASSIGN_C(NA_LOGICAL),                                   \
        MACRO_ASSIGN_C((int)!R_compute_identical(px[flatind_x], py[flatind_y], 0))  \
      )                                                       \
    );         \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)



#endif
