

#ifndef BROADCAST_H
#define BROADCAST_H



#define MACRO_OVERFLOW(REF) ((REF) < intmin || (REF) > intmax)




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




#define MACRO_ACTION_INTEGER1(NACHECK, NACODE, DOLEFT, DORIGHT, DOCODE) do {      \
  if(NACHECK) {                                                   \
  	  NACODE;                                                     \
  }                                                               \
	else {                                                          \
	  e1 = DOLEFT;                                                  \
	  e2 = DORIGHT;                                                 \
	  DOCODE;                                                       \
	}                                                               \
} while(0)




#define MACRO_ACTION_INTEGER2(RULECHECK, RULECODE, NACHECK, NACODE, DOLEFT, DORIGHT, DOCODE) do {      \
  if(RULECHECK) {                                                   \
    RULECODE;                                                       \
  }                                                                 \
  else if(NACHECK) {                                                   \
  	  NACODE;                                                     \
  }                                                               \
	else {                                                          \
	  e1 = DOLEFT;                                                  \
	  e2 = DORIGHT;                                                 \
	  DOCODE;                                                       \
	}                                                               \
} while(0)




#define MACRO_ACTION_INTEGER_MOD1(NACHECK, NACODE, DOCODE) do {      \
  if(NACHECK) {                                                   \
    NACODE;                                                     \
  }                                                             \
	else {                                                          \
	  DOCODE;                                                       \
	}                                                               \
} while(0)




#define MACRO_ACTION_INTEGER_MOD2(NACHECK, RULECHECK, NACODE, RULECODE, DOCODE) do {      \
  if(NACHECK) {                                                   \
    NACODE;                                                     \
  }                                                                 \
  else if(RULECHECK) {                                                   \
    RULECODE;                                                       \
  }                                                                 \
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



#define MACRO_ACTION_BOOLEAN_REL(XREF, YREF, NACODE, DOCODE) do { \
  if(XREF == NA_INTEGER || YREF == NA_INTEGER) {                   \
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


#define MACRO_ACTION_VAPPLY(ASSIGNCODE, ERRORCHECK, ERRORCODE, DOCODE) do { \
  ASSIGNCODE;   \
  if(ERRORCHECK) {  \
    ERRORCODE;  \
  } \
  else {  \
    DOCODE; \
  } \
} while(0)




#define MACRO_TYPESWITCH_DECIMAL_COMMON(DIMCODE, NACODE, DOCODE) do {      \
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




#define MACRO_TYPESWITCH_DECIMAL_CAREFUL(DIMCODE, NACODE, DOCODE) do {      \
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




#define MACRO_TYPESWITCH_DECIMAL_SIMPLE(DIMCODE, DOCODE) do {      \
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




#define MACRO_TYPESWITCH_DECIMAL_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \
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




#define MACRO_TYPESWITCH_DECIMAL_REL(DIMCODE, NACODE1, DOCODE1, NACODE2, DOCODE2) do {      \
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




#define MACRO_TYPESWITCH_INTEGER1(DIMCODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  double e1;                                                \
  double e2;                                                \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER1(                                           \
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        (double)px[flatind_x],                                   \
        (double)py[flatind_y],                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(xint && !yint) {                                  \
    const int *px = INTEGER_RO(x);                                        \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER1(                                           \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \
        NACODE,                                               \
        (double)px[flatind_x],                                   \
        trunc(py[flatind_y]),                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER1(                                           \
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                                     \
        trunc(px[flatind_x]),                                   \
        (double)py[flatind_y],                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER1(                                           \
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        trunc(px[flatind_x]),                                   \
        trunc(py[flatind_y]),                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_INTEGER2(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  double e1;                                                \
  double e2;                                                \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER2(                                           \
        RULECHECK,                                                      \
        RULECODE,                                                       \
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                               \
        (double)px[flatind_x],                                   \
        (double)py[flatind_y],                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(xint && !yint) {                                  \
    const int *px = INTEGER_RO(x);                                        \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER2(                                           \
        RULECHECK,                                                      \
        RULECODE,                                                       \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \
        NACODE,                                               \
        (double)px[flatind_x],                                   \
        trunc(py[flatind_y]),                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER2(                                           \
        RULECHECK,                                                      \
        RULECODE,                                                       \
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
        NACODE,                                                     \
        trunc(px[flatind_x]),                                   \
        (double)py[flatind_y],                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER2(                                           \
        RULECHECK,                                                      \
        RULECODE,                                                       \
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
        NACODE,                                               \
        trunc(px[flatind_x]),                                   \
        trunc(py[flatind_y]),                                   \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_TYPESWITCH_INTEGER_MOD(DIMCODE, NACODE, RULECODE, DOCODE) do {      \
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \
  if(xint && yint) {                                        \
    const int *px = INTEGER_RO(x);                                        \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER_MOD1(                                           \
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
      MACRO_ACTION_INTEGER_MOD2(                                           \
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \
        MACRO_OVERFLOW(py[flatind_y]),           \
        NACODE,                                               \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && yint) {                                  \
    const double *px = REAL_RO(x);                                           \
    const int *py = INTEGER_RO(y);                                        \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER_MOD2(                                           \
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \
        MACRO_OVERFLOW(px[flatind_x]),           \
        NACODE,                                               \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
  else if(!xint && !yint) {                                 \
    const double *px = REAL_RO(x);                                           \
    const double *py = REAL_RO(y);                                           \
    DIMCODE(                                                          \
      MACRO_ACTION_INTEGER_MOD2(                                           \
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \
        MACRO_OVERFLOW(px[flatind_x]) || MACRO_OVERFLOW(py[flatind_y]),           \
        NACODE,                                               \
        NACODE,                                               \
        DOCODE                                                \
      )                                                       \
    );                                                       \
  }                                                         \
} while(0)




#define MACRO_ASSIGN_C(INPUTCODE) do {  \
  tempout = INPUTCODE;              \
  pout[flatind_out] = tempout;      \
} while(0)


#define MACRO_OP_DEC_MATH(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_COMMON(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)px[flatind_x] / (double)py[flatind_y])	\
      );	\
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_SPECIAL(	\
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
      MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\
      );	\
      break;	\
    }	\
    case 7:	\
    {	\
      MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
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


#define MACRO_OP_DEC_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] == py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 2:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] != py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 3:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] < py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 4:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] > py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 5:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] <= py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 6:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(px[flatind_x] >= py[flatind_y])  \
    );	\
    break;	\
  }	\
  case 7:	\
  {	\
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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
    MACRO_TYPESWITCH_DECIMAL_REL(	\
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


#define MACRO_OP_INT_MATH(DIMCODE) do {	\
  double intmax = pow(2, 53);           \
  double intmin = -1 * intmax;          \
  switch(op) {	\
    case 1:	\
    {	\
      if(!rcpp_int53_need_guard1(x, y)) {    \
        MACRO_TYPESWITCH_DECIMAL_COMMON(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y])	\
        );	\
      }   \
      else {    \
        MACRO_TYPESWITCH_INTEGER1(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 + e2, intmin, intmax))	\
        );	\
      }   \
      break;	\
    }	\
    case 2:	\
    {	\
      if(!rcpp_int53_need_guard1(x, y)) {    \
        MACRO_TYPESWITCH_DECIMAL_COMMON(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y])	\
        );	\
      }   \
      else {    \
        MACRO_TYPESWITCH_INTEGER1(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 - e2, intmin, intmax))	\
        );	\
      }   \
      break;	\
    }	\
    case 3:	\
    {	\
      if(!rcpp_int53_need_guard2(x, y)) {    \
        MACRO_TYPESWITCH_DECIMAL_COMMON(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y])	\
        );	\
      }   \
      else {    \
        MACRO_TYPESWITCH_INTEGER1(	\
          DIMCODE,	\
          MACRO_ASSIGN_C(NA_REAL),	\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 * e2, intmin, intmax))	\
        );	\
      }   \
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_TYPESWITCH_INTEGER_MOD(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((double)rcpp_gcd(px[flatind_x], py[flatind_y]))	\
      );    \
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_TYPESWITCH_INTEGER2(	\
        DIMCODE,	\
        trunc(px[flatind_x]) == 1 || trunc(py[flatind_y]) == 0,	\
        MACRO_ASSIGN_C(1),	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C(rcpp_int53_guard(trunc(R_pow(e1, e2)), intmin, intmax))	\
      );	\
      break;	\
    }	\
    case 6:	\
    {	\
      MACRO_TYPESWITCH_INTEGER1(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((e1 < e2) ? e1 : e2) 	\
      );	\
      break;	\
    }	\
    case 7:	\
    {	\
      MACRO_TYPESWITCH_INTEGER1(	\
        DIMCODE,	\
        MACRO_ASSIGN_C(NA_REAL),	\
        MACRO_ASSIGN_C((e1 > e2) ? e1 : e2) 	\
      );	\
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  }	\
} while(0)


#define MACRO_OP_INT_REL(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 == e2)  \
    );	\
    break;	\
  }	\
  case 2:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 != e2)  \
    );	\
    break;	\
  }	\
  case 3:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 < e2)  \
    );	\
    break;	\
  }	\
  case 4:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 > e2)  \
    );	\
    break;	\
  }	\
  case 5:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 <= e2)  \
    );	\
    break;	\
  }	\
  case 6:	\
  {	\
    MACRO_TYPESWITCH_INTEGER1(	\
      DIMCODE,	\
      MACRO_ASSIGN_C(NA_LOGICAL), \
      MACRO_ASSIGN_C(e1 >= e2)  \
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
    case 5:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] == (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 6:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 7:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] < (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 8:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] > (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 9:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] <= (bool)py[flatind_y])  \
        )                                                       \
      );                                                        \
      break;	\
    }	\
    case 10:	\
    {	\
      DIMCODE(                                                          \
        MACRO_ACTION_BOOLEAN_REL(                                           \
          px[flatind_x], py[flatind_y],       \
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \
          MACRO_ASSIGN_C((bool)px[flatind_x] >= (bool)py[flatind_y])  \
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
      out[flatind_out] = rcpp_string_plus(x[flatind_x], y[flatind_y]) \
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


#define MACRO_OP_STR_DIST(DIMCODE) do {	\
  switch(op) {	\
  case 1:	\
  {	\
    DIMCODE(                                                          \
      pout[flatind_out] = rcpp_str_dist_led(x[flatind_x], y[flatind_y])   \
    );                                                                \
    break;	\
  }	\
  default:	\
  {	\
    stop("given operator not supported in the given context");	\
  }	\
}	\
} while(0)


#define MACRO_OP_IFELSE(DIMCODE) do {       \
  switch(TYPEOF(x)) {	\
    case LGLSXP:	\
    {	\
      const int *px = LOGICAL_RO(x);	\
      const int *py = LOGICAL_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));	\
      int *pout;	\
      pout = LOGICAL(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          pout[flatind_out] = NA_LOGICAL,	\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case INTSXP:	\
    {	\
      const int *px = INTEGER_RO(x);	\
      const int *py = INTEGER_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));	\
      int *pout;	\
      pout = INTEGER(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          pout[flatind_out] = NA_INTEGER,	\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case REALSXP:	\
    {	\
      const double *px = REAL_RO(x);	\
      const double *py = REAL_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));	\
      double *pout;	\
      pout = REAL(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          pout[flatind_out] = NA_REAL,	\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case CPLXSXP:	\
    {	\
      const Rcomplex *px = COMPLEX_RO(x);	\
      const Rcomplex *py = COMPLEX_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));	\
      Rcomplex *pout;	\
      pout = COMPLEX(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          pout[flatind_out] = rcpp_cplx_returnNA(),	\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *px = STRING_PTR_RO(x);	\
      const SEXP *py = STRING_PTR_RO(y);	\
      	\
      SEXP out = PROTECT(Rf_allocVector(STRSXP, nout));	\
      SEXP *pout;	\
      pout = STRING_PTR(out);	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          pout[flatind_out] = NA_STRING,	\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    case VECSXP:	\
    {	\
      SEXP out = PROTECT(Rf_allocVector(VECSXP, nout));	\
      	\
      DIMCODE(	\
        MACRO_ACTION2(	\
          pcond[flatind_out] == NA_LOGICAL,	\
          SET_VECTOR_ELT(out, flatind_out, R_NilValue),	\
          SET_VECTOR_ELT(out, flatind_out, pcond[flatind_out] ? VECTOR_ELT(x, flatind_x) : VECTOR_ELT(y, flatind_y))	\
        )	\
      );	\
      	\
      UNPROTECT(1);	\
      return out;	\
      	\
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
} while(0)


#define MACRO_OP_BCAPPLY(DIMCODE) do {       \
  switch(TYPEOF(out)) {	\
    case LGLSXP:	\
    {	\
      int *pout;	\
      pout = LOGICAL(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = LOGICAL(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case INTSXP:	\
    {	\
      int *pout;	\
      pout = INTEGER(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = INTEGER(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case REALSXP:	\
    {	\
      double *pout;	\
      pout = REAL(out);	\
      DIMCODE(	\
        pout[flatind_out] = REAL(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case CPLXSXP:	\
    {	\
      Rcomplex *pout;	\
      pout = COMPLEX(out);	\
      DIMCODE(	\
        pout[flatind_out] = COMPLEX(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case STRSXP:	\
    {	\
      SEXP *pout;	\
      pout = STRING_PTR(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = STRING_PTR(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case RAWSXP:	\
    {	\
      Rbyte *pout;	\
      pout = RAW(out);	\
      	\
      DIMCODE(	\
        pout[flatind_out] = RAW(f(x, y, flatind_x + 1, flatind_y + 1))[0] \
      );	\
      break; \
    }	\
    case VECSXP:	\
    {	\
     DIMCODE(	\
        SET_VECTOR_ELT(out, flatind_out, f(x, y, flatind_x + 1, flatind_y + 1))	\
      );	\
      break; \
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
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

#define MACRO_DIM_BIND_2(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2;              \
  R_xlen_t i_x2;                \
  	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2;           \
        DOCODE;                         \
  	 }	\
	 }	\
} while(0)





#define MACRO_DIM_BIND_4(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4;              \
  R_xlen_t i_x2, i_x3, i_x4;                \
  	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4;           \
        DOCODE;                         \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)





#define MACRO_DIM_BIND_6(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6;                \
  	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6;           \
        DOCODE;                         \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)





#define MACRO_DIM_BIND_8(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8;                \
  	 for(int iter8 = pstart[7]; iter8 <= pend[7]; ++iter8) {	\
i_out8 = iter8 * pdcp_out[7];	\
i_x8 = pby_x[7] * (iter8 - pstart[7]) * pdcp_x[7];	\
	 for(int iter7 = pstart[6]; iter7 <= pend[6]; ++iter7) {	\
i_out7 = iter7 * pdcp_out[6];	\
i_x7 = pby_x[6] * (iter7 - pstart[6]) * pdcp_x[6];	\
	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8;           \
        DOCODE;                         \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)





#define MACRO_DIM_BIND_10(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8, i_out9, i_out10;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10;                \
  	 for(int iter10 = pstart[9]; iter10 <= pend[9]; ++iter10) {	\
i_out10 = iter10 * pdcp_out[9];	\
i_x10 = pby_x[9] * (iter10 - pstart[9]) * pdcp_x[9];	\
	 for(int iter9 = pstart[8]; iter9 <= pend[8]; ++iter9) {	\
i_out9 = iter9 * pdcp_out[8];	\
i_x9 = pby_x[8] * (iter9 - pstart[8]) * pdcp_x[8];	\
	 for(int iter8 = pstart[7]; iter8 <= pend[7]; ++iter8) {	\
i_out8 = iter8 * pdcp_out[7];	\
i_x8 = pby_x[7] * (iter8 - pstart[7]) * pdcp_x[7];	\
	 for(int iter7 = pstart[6]; iter7 <= pend[6]; ++iter7) {	\
i_out7 = iter7 * pdcp_out[6];	\
i_x7 = pby_x[6] * (iter7 - pstart[6]) * pdcp_x[6];	\
	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8 + i_out9 + i_out10;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10;           \
        DOCODE;                         \
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





#define MACRO_DIM_BIND_12(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8, i_out9, i_out10, i_out11, i_out12;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12;                \
  	 for(int iter12 = pstart[11]; iter12 <= pend[11]; ++iter12) {	\
i_out12 = iter12 * pdcp_out[11];	\
i_x12 = pby_x[11] * (iter12 - pstart[11]) * pdcp_x[11];	\
	 for(int iter11 = pstart[10]; iter11 <= pend[10]; ++iter11) {	\
i_out11 = iter11 * pdcp_out[10];	\
i_x11 = pby_x[10] * (iter11 - pstart[10]) * pdcp_x[10];	\
	 for(int iter10 = pstart[9]; iter10 <= pend[9]; ++iter10) {	\
i_out10 = iter10 * pdcp_out[9];	\
i_x10 = pby_x[9] * (iter10 - pstart[9]) * pdcp_x[9];	\
	 for(int iter9 = pstart[8]; iter9 <= pend[8]; ++iter9) {	\
i_out9 = iter9 * pdcp_out[8];	\
i_x9 = pby_x[8] * (iter9 - pstart[8]) * pdcp_x[8];	\
	 for(int iter8 = pstart[7]; iter8 <= pend[7]; ++iter8) {	\
i_out8 = iter8 * pdcp_out[7];	\
i_x8 = pby_x[7] * (iter8 - pstart[7]) * pdcp_x[7];	\
	 for(int iter7 = pstart[6]; iter7 <= pend[6]; ++iter7) {	\
i_out7 = iter7 * pdcp_out[6];	\
i_x7 = pby_x[6] * (iter7 - pstart[6]) * pdcp_x[6];	\
	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8 + i_out9 + i_out10 + i_out11 + i_out12;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12;           \
        DOCODE;                         \
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





#define MACRO_DIM_BIND_14(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8, i_out9, i_out10, i_out11, i_out12, i_out13, i_out14;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14;                \
  	 for(int iter14 = pstart[13]; iter14 <= pend[13]; ++iter14) {	\
i_out14 = iter14 * pdcp_out[13];	\
i_x14 = pby_x[13] * (iter14 - pstart[13]) * pdcp_x[13];	\
	 for(int iter13 = pstart[12]; iter13 <= pend[12]; ++iter13) {	\
i_out13 = iter13 * pdcp_out[12];	\
i_x13 = pby_x[12] * (iter13 - pstart[12]) * pdcp_x[12];	\
	 for(int iter12 = pstart[11]; iter12 <= pend[11]; ++iter12) {	\
i_out12 = iter12 * pdcp_out[11];	\
i_x12 = pby_x[11] * (iter12 - pstart[11]) * pdcp_x[11];	\
	 for(int iter11 = pstart[10]; iter11 <= pend[10]; ++iter11) {	\
i_out11 = iter11 * pdcp_out[10];	\
i_x11 = pby_x[10] * (iter11 - pstart[10]) * pdcp_x[10];	\
	 for(int iter10 = pstart[9]; iter10 <= pend[9]; ++iter10) {	\
i_out10 = iter10 * pdcp_out[9];	\
i_x10 = pby_x[9] * (iter10 - pstart[9]) * pdcp_x[9];	\
	 for(int iter9 = pstart[8]; iter9 <= pend[8]; ++iter9) {	\
i_out9 = iter9 * pdcp_out[8];	\
i_x9 = pby_x[8] * (iter9 - pstart[8]) * pdcp_x[8];	\
	 for(int iter8 = pstart[7]; iter8 <= pend[7]; ++iter8) {	\
i_out8 = iter8 * pdcp_out[7];	\
i_x8 = pby_x[7] * (iter8 - pstart[7]) * pdcp_x[7];	\
	 for(int iter7 = pstart[6]; iter7 <= pend[6]; ++iter7) {	\
i_out7 = iter7 * pdcp_out[6];	\
i_x7 = pby_x[6] * (iter7 - pstart[6]) * pdcp_x[6];	\
	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8 + i_out9 + i_out10 + i_out11 + i_out12 + i_out13 + i_out14;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14;           \
        DOCODE;                         \
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





#define MACRO_DIM_BIND_16(DOCODE) do {  \
  double *pdcp_out = REAL(dcp_out);  \
  double *pdcp_x = REAL(dcp_x);  \
                                        \
  const int *pby_x = INTEGER_RO(by_x);  \
  const int *pstart = INTEGER_RO(starts); \
  const int *pend = INTEGER_RO(ends);    \
  R_xlen_t flatind_out;                 \
  R_xlen_t flatind_x;                   \
  R_xlen_t i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8, i_out9, i_out10, i_out11, i_out12, i_out13, i_out14, i_out15, i_out16;              \
  R_xlen_t i_x2, i_x3, i_x4, i_x5, i_x6, i_x7, i_x8, i_x9, i_x10, i_x11, i_x12, i_x13, i_x14, i_x15, i_x16;                \
  	 for(int iter16 = pstart[15]; iter16 <= pend[15]; ++iter16) {	\
i_out16 = iter16 * pdcp_out[15];	\
i_x16 = pby_x[15] * (iter16 - pstart[15]) * pdcp_x[15];	\
	 for(int iter15 = pstart[14]; iter15 <= pend[14]; ++iter15) {	\
i_out15 = iter15 * pdcp_out[14];	\
i_x15 = pby_x[14] * (iter15 - pstart[14]) * pdcp_x[14];	\
	 for(int iter14 = pstart[13]; iter14 <= pend[13]; ++iter14) {	\
i_out14 = iter14 * pdcp_out[13];	\
i_x14 = pby_x[13] * (iter14 - pstart[13]) * pdcp_x[13];	\
	 for(int iter13 = pstart[12]; iter13 <= pend[12]; ++iter13) {	\
i_out13 = iter13 * pdcp_out[12];	\
i_x13 = pby_x[12] * (iter13 - pstart[12]) * pdcp_x[12];	\
	 for(int iter12 = pstart[11]; iter12 <= pend[11]; ++iter12) {	\
i_out12 = iter12 * pdcp_out[11];	\
i_x12 = pby_x[11] * (iter12 - pstart[11]) * pdcp_x[11];	\
	 for(int iter11 = pstart[10]; iter11 <= pend[10]; ++iter11) {	\
i_out11 = iter11 * pdcp_out[10];	\
i_x11 = pby_x[10] * (iter11 - pstart[10]) * pdcp_x[10];	\
	 for(int iter10 = pstart[9]; iter10 <= pend[9]; ++iter10) {	\
i_out10 = iter10 * pdcp_out[9];	\
i_x10 = pby_x[9] * (iter10 - pstart[9]) * pdcp_x[9];	\
	 for(int iter9 = pstart[8]; iter9 <= pend[8]; ++iter9) {	\
i_out9 = iter9 * pdcp_out[8];	\
i_x9 = pby_x[8] * (iter9 - pstart[8]) * pdcp_x[8];	\
	 for(int iter8 = pstart[7]; iter8 <= pend[7]; ++iter8) {	\
i_out8 = iter8 * pdcp_out[7];	\
i_x8 = pby_x[7] * (iter8 - pstart[7]) * pdcp_x[7];	\
	 for(int iter7 = pstart[6]; iter7 <= pend[6]; ++iter7) {	\
i_out7 = iter7 * pdcp_out[6];	\
i_x7 = pby_x[6] * (iter7 - pstart[6]) * pdcp_x[6];	\
	 for(int iter6 = pstart[5]; iter6 <= pend[5]; ++iter6) {	\
i_out6 = iter6 * pdcp_out[5];	\
i_x6 = pby_x[5] * (iter6 - pstart[5]) * pdcp_x[5];	\
	 for(int iter5 = pstart[4]; iter5 <= pend[4]; ++iter5) {	\
i_out5 = iter5 * pdcp_out[4];	\
i_x5 = pby_x[4] * (iter5 - pstart[4]) * pdcp_x[4];	\
	 for(int iter4 = pstart[3]; iter4 <= pend[3]; ++iter4) {	\
i_out4 = iter4 * pdcp_out[3];	\
i_x4 = pby_x[3] * (iter4 - pstart[3]) * pdcp_x[3];	\
	 for(int iter3 = pstart[2]; iter3 <= pend[2]; ++iter3) {	\
i_out3 = iter3 * pdcp_out[2];	\
i_x3 = pby_x[2] * (iter3 - pstart[2]) * pdcp_x[2];	\
	 for(int iter2 = pstart[1]; iter2 <= pend[1]; ++iter2) {	\
i_out2 = iter2 * pdcp_out[1];	\
i_x2 = pby_x[1] * (iter2 - pstart[1]) * pdcp_x[1];	\
	 for(int iter1 = pstart[0]; iter1 <= pend[0]; ++iter1) {	\
	\
	\
        flatind_out = iter1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8 + i_out9 + i_out10 + i_out11 + i_out12 + i_out13 + i_out14 + i_out15 + i_out16;       \
        flatind_x = pby_x[0] * (iter1 - pstart[0]) + i_x2 + i_x3 + i_x4 + i_x5 + i_x6 + i_x7 + i_x8 + i_x9 + i_x10 + i_x11 + i_x12 + i_x13 + i_x14 + i_x15 + i_x16;           \
        DOCODE;                         \
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






#define MACRO_DIM_BIND_DOCALL(DOCODE) do {     \
  int ndims = Rf_length(out_dim);         \
                                          \
  switch(ndims) {       \
    case 2:                                       \
  MACRO_DIM_BIND_2(DOCODE);    \
  break;                                        \
case 4:                                       \
  MACRO_DIM_BIND_4(DOCODE);    \
  break;                                        \
case 6:                                       \
  MACRO_DIM_BIND_6(DOCODE);    \
  break;                                        \
case 8:                                       \
  MACRO_DIM_BIND_8(DOCODE);    \
  break;                                        \
case 10:                                       \
  MACRO_DIM_BIND_10(DOCODE);    \
  break;                                        \
case 12:                                       \
  MACRO_DIM_BIND_12(DOCODE);    \
  break;                                        \
case 14:                                       \
  MACRO_DIM_BIND_14(DOCODE);    \
  break;                                        \
case 16:                                       \
  MACRO_DIM_BIND_16(DOCODE);    \
  break;                                        \
       \
  }       \
} while(0)



#define MACRO_DIM_ACAST(DOCODE) do {              \
  if(Rf_length(subs) < 16) {                     \
    stop("subs not of length 16");              \
  }                                               \
  if(Rf_length(starts) < 16) {                   \
    stop("starts not of length 16");            \
  }                                               \
  if(Rf_length(lens) < 16) {                     \
    stop("lens not of length 16");              \
  }                                               \
  if(Rf_length(dcp_y) < 16) {                    \
    stop("dcp_y not of length 16");             \
  }                                               \
  if(Rf_length(dcp_out) < 16) {                  \
    stop("dcp_out not of length 16");           \
  }                                               \
                                                  \
  const double *pdcp_out = REAL_RO(dcp_out);      \
  const double *pdcp_y = REAL_RO(dcp_y);          \
                                                  \
  const int *pstarts = INTEGER_RO(starts);        \
                                                  \
                                                  \
  const int N1 = INTEGER(lens)[0];	\
const int N2 = INTEGER(lens)[1];	\
const int N3 = INTEGER(lens)[2];	\
const int N4 = INTEGER(lens)[3];	\
const int N5 = INTEGER(lens)[4];	\
const int N6 = INTEGER(lens)[5];	\
const int N7 = INTEGER(lens)[6];	\
const int N8 = INTEGER(lens)[7];	\
const int N9 = INTEGER(lens)[8];	\
const int N10 = INTEGER(lens)[9];	\
const int N11 = INTEGER(lens)[10];	\
const int N12 = INTEGER(lens)[11];	\
const int N13 = INTEGER(lens)[12];	\
const int N14 = INTEGER(lens)[13];	\
const int N15 = INTEGER(lens)[14];	\
const int N16 = INTEGER(lens)[15];	\
                                                  \
  const SEXP ind1 = VECTOR_ELT(subs, 0);	\
const SEXP ind2 = VECTOR_ELT(subs, 1);	\
const SEXP ind3 = VECTOR_ELT(subs, 2);	\
const SEXP ind4 = VECTOR_ELT(subs, 3);	\
const SEXP ind5 = VECTOR_ELT(subs, 4);	\
const SEXP ind6 = VECTOR_ELT(subs, 5);	\
const SEXP ind7 = VECTOR_ELT(subs, 6);	\
const SEXP ind8 = VECTOR_ELT(subs, 7);	\
const SEXP ind9 = VECTOR_ELT(subs, 8);	\
const SEXP ind10 = VECTOR_ELT(subs, 9);	\
const SEXP ind11 = VECTOR_ELT(subs, 10);	\
const SEXP ind12 = VECTOR_ELT(subs, 11);	\
const SEXP ind13 = VECTOR_ELT(subs, 12);	\
const SEXP ind14 = VECTOR_ELT(subs, 13);	\
const SEXP ind15 = VECTOR_ELT(subs, 14);	\
const SEXP ind16 = VECTOR_ELT(subs, 15);	\
                                                  \
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
const int *pind13 = INTEGER_RO(ind13);	\
const int *pind14 = INTEGER_RO(ind14);	\
const int *pind15 = INTEGER_RO(ind15);	\
const int *pind16 = INTEGER_RO(ind16);	\
                                                  \
  R_xlen_t flatind_out;       \
  R_xlen_t flatind_y;       \
  R_xlen_t i_out1, i_out2, i_out3, i_out4, i_out5, i_out6, i_out7, i_out8, i_out9, i_out10, i_out11, i_out12, i_out13, i_out14, i_out15, i_out16; \
  R_xlen_t i_y1, i_y2, i_y3, i_y4, i_y5, i_y6, i_y7, i_y8, i_y9, i_y10, i_y11, i_y12, i_y13, i_y14, i_y15, i_y16; \
  	 for(int iter16 = 0; iter16 < N16; ++iter16) {	\
i_out16 = (pstarts[15] + iter16) * pdcp_out[15];	\
i_y16 = (pind16[iter16] - 1) * pdcp_y[15];	\
	 for(int iter15 = 0; iter15 < N15; ++iter15) {	\
i_out15 = (pstarts[14] + iter15) * pdcp_out[14];	\
i_y15 = (pind15[iter15] - 1) * pdcp_y[14];	\
	 for(int iter14 = 0; iter14 < N14; ++iter14) {	\
i_out14 = (pstarts[13] + iter14) * pdcp_out[13];	\
i_y14 = (pind14[iter14] - 1) * pdcp_y[13];	\
	 for(int iter13 = 0; iter13 < N13; ++iter13) {	\
i_out13 = (pstarts[12] + iter13) * pdcp_out[12];	\
i_y13 = (pind13[iter13] - 1) * pdcp_y[12];	\
	 for(int iter12 = 0; iter12 < N12; ++iter12) {	\
i_out12 = (pstarts[11] + iter12) * pdcp_out[11];	\
i_y12 = (pind12[iter12] - 1) * pdcp_y[11];	\
	 for(int iter11 = 0; iter11 < N11; ++iter11) {	\
i_out11 = (pstarts[10] + iter11) * pdcp_out[10];	\
i_y11 = (pind11[iter11] - 1) * pdcp_y[10];	\
	 for(int iter10 = 0; iter10 < N10; ++iter10) {	\
i_out10 = (pstarts[9] + iter10) * pdcp_out[9];	\
i_y10 = (pind10[iter10] - 1) * pdcp_y[9];	\
	 for(int iter9 = 0; iter9 < N9; ++iter9) {	\
i_out9 = (pstarts[8] + iter9) * pdcp_out[8];	\
i_y9 = (pind9[iter9] - 1) * pdcp_y[8];	\
	 for(int iter8 = 0; iter8 < N8; ++iter8) {	\
i_out8 = (pstarts[7] + iter8) * pdcp_out[7];	\
i_y8 = (pind8[iter8] - 1) * pdcp_y[7];	\
	 for(int iter7 = 0; iter7 < N7; ++iter7) {	\
i_out7 = (pstarts[6] + iter7) * pdcp_out[6];	\
i_y7 = (pind7[iter7] - 1) * pdcp_y[6];	\
	 for(int iter6 = 0; iter6 < N6; ++iter6) {	\
i_out6 = (pstarts[5] + iter6) * pdcp_out[5];	\
i_y6 = (pind6[iter6] - 1) * pdcp_y[5];	\
	 for(int iter5 = 0; iter5 < N5; ++iter5) {	\
i_out5 = (pstarts[4] + iter5) * pdcp_out[4];	\
i_y5 = (pind5[iter5] - 1) * pdcp_y[4];	\
	 for(int iter4 = 0; iter4 < N4; ++iter4) {	\
i_out4 = (pstarts[3] + iter4) * pdcp_out[3];	\
i_y4 = (pind4[iter4] - 1) * pdcp_y[3];	\
	 for(int iter3 = 0; iter3 < N3; ++iter3) {	\
i_out3 = (pstarts[2] + iter3) * pdcp_out[2];	\
i_y3 = (pind3[iter3] - 1) * pdcp_y[2];	\
	 for(int iter2 = 0; iter2 < N2; ++iter2) {	\
i_out2 = (pstarts[1] + iter2) * pdcp_out[1];	\
i_y2 = (pind2[iter2] - 1) * pdcp_y[1];	\
	 for(int iter1 = 0; iter1 < N1; ++iter1) {	\
i_out1 = (pstarts[0] + iter1) * pdcp_out[0];	\
i_y1 = (pind1[iter1] - 1) * pdcp_y[0];	\
        flatind_out = i_out1 + i_out2 + i_out3 + i_out4 + i_out5 + i_out6 + i_out7 + i_out8 + i_out9 + i_out10 + i_out11 + i_out12 + i_out13 + i_out14 + i_out15 + i_out16;       \
        flatind_y = i_y1 + i_y2 + i_y3 + i_y4 + i_y5 + i_y6 + i_y7 + i_y8 + i_y9 + i_y10 + i_y11 + i_y12 + i_y13 + i_y14 + i_y15 + i_y16;     \
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



#define MACRO_OP_ACAST_LOOP(DOCODE) do {                  \
                                                            \
  for(int i = 0; i < grp_n; ++i) {                          \
    grp_count = rcpp_factor_count(grp, i + 1);              \
    if(grp_count > 0) {                                       \
      grp_which = rcpp_factor_which(grp, i + 1, grp_count);   \
      plens[margin] = grp_count;                              \
      SET_VECTOR_ELT(subs, margin, grp_which);                \
      DOCODE;                                                 \
      pstarts[newdim] = pstarts[newdim] + 1;                  \
    }                                                         \
  }                                                         \
} while(0)


#define MACRO_OP_ACAST do {       \
  switch(TYPEOF(out)) {	\
    case LGLSXP:	\
    {	\
      int *py = LOGICAL(y);                                 \
      int *pout = LOGICAL(out);                             \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \
      break;                                                \
    }	\
    case INTSXP:	\
    {	\
      int *py = INTEGER(y);                                 \
      int *pout = INTEGER(out);                             \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \
      break;                                                \
    }	\
    case REALSXP:	\
    {	\
      double *py = REAL(y);                                     \
      double *pout = REAL(out);                                 \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));       \
      break;                                                \
    }	\
    case CPLXSXP:	\
    {	\
      Rcomplex *py = COMPLEX(y);                                 \
      Rcomplex *pout = COMPLEX(out);                             \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \
      break;                                                \
    }	\
    case STRSXP:	\
    {	\
      SEXP *py = STRING_PTR(y);                                 \
      SEXP *pout = STRING_PTR(out);                             \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \
      break;                                                \
    }	\
    case RAWSXP:	\
    {	\
      Rbyte *py = RAW(y);	                                  \
      Rbyte *pout = RAW(out);	                              \
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \
      break;                                                \
    }	\
    case VECSXP:	\
    {	\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(                                                \
        SET_VECTOR_ELT(out, flatind_out, VECTOR_ELT(y, flatind_y))  \
      ));                                                              \
      break;                                                \
    }	\
    default:	\
    {	\
      stop("unsupported type");	\
    }	\
  }	\
} while(0)


#endif
