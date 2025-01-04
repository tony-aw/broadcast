

library(stringi)

################################################################################
# Numeric ====
#

macro_typeswitch_numeric_common <- "

#define MACRO_TYPESWITCH_NUMERIC_COMMON(DIMCODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        px[flatind_x] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION1(                                           \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"


macro_typeswitch_numeric_careful <- "

#define MACRO_TYPESWITCH_NUMERIC_CAREFUL(DIMCODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"




macro_typeswitch_numeric_simple <- "

#define MACRO_TYPESWITCH_NUMERIC_SIMPLE(DIMCODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(DOCODE);                                                      \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(DOCODE);                                                      \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(DOCODE);                                                      \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(DOCODE);                                                      \\
  }                                                         \\
} while(0)

"



macro_typeswitch_numeric_special <- "

#define MACRO_TYPESWITCH_NUMERIC_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION4(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION4(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        px[flatind_x] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION4(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION3(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"


macro_typeswitch_numeric_rel <- "

#define MACRO_TYPESWITCH_NUMERIC_REL(DIMCODE, NACODE1, DOCODE1, NACODE2, DOCODE2) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                \\
    const int *py = INTEGER_RO(y);                                \\
    DIMCODE(                                                      \\
      MACRO_DOUBLEPASS(                                           \\
        MACRO_ACTION2(                                           \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
          NACODE1,                                               \\
          DOCODE1                                                \\
        ),                                                      \\
        MACRO_ACTION2(                                          \\
          R_isnancpp(tempcalc),                                   \\
          NACODE2,                                               \\
          DOCODE2                                               \\
        )                                                       \\
      )                                                         \\
    );                                                          \\
  }                                                             \\
  else if(xint && !yint) {                                      \\
    const int *px = INTEGER_RO(x);                              \\
    const double *py = REAL_RO(y);                              \\
    DIMCODE(                                                    \\
      MACRO_DOUBLEPASS(                                         \\
        MACRO_ACTION2(                                           \\
          px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \\
          NACODE1,                                               \\
          DOCODE1                                                \\
        ),                                                      \\
        MACRO_ACTION2(                                          \\
          R_isnancpp(tempcalc),                                   \\
          NACODE2,                                               \\
          DOCODE2                                               \\
        )                                                       \\
      )                                                         \\
    );                                                          \\
  }                                                             \\
  else if(!xint && yint) {                                      \\
    const double *px = REAL_RO(x);                              \\
    const int *py = INTEGER_RO(y);                              \\
    DIMCODE(                                                    \\
      MACRO_DOUBLEPASS(                                         \\
        MACRO_ACTION2(                                           \\
          R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \\
          NACODE1,                                               \\
          DOCODE1                                                \\
        ),                                                      \\
        MACRO_ACTION2(                                          \\
          R_isnancpp(tempcalc),                                   \\
          NACODE2,                                               \\
          DOCODE2                                               \\
        )                                                       \\
      )                                                         \\
    );                                                          \\
  }                                                             \\
  else if(!xint && !yint) {                                     \\
    const double *px = REAL_RO(x);                              \\
    const double *py = REAL_RO(y);                              \\
    DIMCODE(                                                    \\
      MACRO_DOUBLEPASS(                                         \\
        MACRO_ACTION2(                                           \\
          R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
          NACODE1,                                               \\
          DOCODE1                                                \\
        ),                                                      \\
        MACRO_ACTION2(                                          \\
          R_isnancpp(tempcalc),                                   \\
          NACODE2,                                               \\
          DOCODE2                                               \\
        )                                                       \\
      )                                                         \\
    );                                                       \\
  }                                                         \\
} while(0)

"


macro_typeswitch_numeric <- stri_c(
  macro_typeswitch_numeric_common,
  "\n",
  macro_typeswitch_numeric_careful,
  "\n",
  macro_typeswitch_numeric_simple,
  "\n",
  macro_typeswitch_numeric_special,
  "\n",
  macro_typeswitch_numeric_rel
)

readr::write_file(macro_typeswitch_numeric, "macro_typeswitch_numeric.txt")
