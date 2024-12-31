
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
      MACRO_ACTION_COMMON(                                           \\
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
      MACRO_ACTION_COMMON(                                           \\
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_COMMON(                                           \\
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
      MACRO_ACTION_COMMON(                                           \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"


readr::write_file(macro_typeswitch_numeric_common, "macro_typeswitch_numeric_common.txt")




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


readr::write_file(macro_typeswitch_numeric_simple, "macro_typeswitch_numeric_simple.txt")



macro_typeswitch_numeric_special <- "

#define MACRO_TYPESWITCH_NUMERIC_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_SPECIAL(                                           \\
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
      MACRO_ACTION_SPECIAL(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_SPECIAL(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
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
      MACRO_ACTION_SPECIAL(                                           \\
        RULECHECK,                                                    \\
        RULECODE,                                                     \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"


readr::write_file(macro_typeswitch_numeric_special, "macro_typeswitch_numeric_special.txt")
