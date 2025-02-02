

library(stringi)


################################################################################
# Integer ====
#


macro_typeswitch_integer1 <- "

#define MACRO_TYPESWITCH_INTEGER1(DIMCODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  double e1;                                                \\
  double e2;                                                \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER1(                                           \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        (double)px[flatind_x],                                   \\
        (double)py[flatind_y],                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER1(                                           \\
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \\
        NACODE,                                               \\
        (double)px[flatind_x],                                   \\
        trunc(py[flatind_y]),                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER1(                                           \\
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \\
        trunc(px[flatind_x]),                                   \\
        (double)py[flatind_y],                                   \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER1(                                           \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        trunc(px[flatind_x]),                                   \\
        trunc(py[flatind_y]),                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"




macro_typeswitch_integer2 <- "

#define MACRO_TYPESWITCH_INTEGER2(DIMCODE, NACODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  double e1;                                                \\
  double e2;                                                \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER2(                                           \\
        RULECHECK,                                                      \\
        RULECODE,                                                       \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        (double)px[flatind_x],                                   \\
        (double)py[flatind_y],                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(xint && !yint) {                                  \\
    const int *px = INTEGER_RO(x);                                        \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER2(                                           \\
        RULECHECK,                                                      \\
        RULECODE,                                                       \\
        px[flatind_x] == NA_INTEGER || R_isnancpp(py[flatind_y]), \\
        NACODE,                                               \\
        (double)px[flatind_x],                                   \\
        trunc(py[flatind_y]),                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && yint) {                                  \\
    const double *px = REAL_RO(x);                                           \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER2(                                           \\
        RULECHECK,                                                      \\
        RULECODE,                                                       \\
        R_isnancpp(px[flatind_x]) || py[flatind_y] == NA_INTEGER,  \\
        trunc(px[flatind_x]),                                   \\
        (double)py[flatind_y],                                   \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER2(                                           \\
        RULECHECK,                                                      \\
        RULECODE,                                                       \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        NACODE,                                               \\
        trunc(px[flatind_x]),                                   \\
        trunc(py[flatind_y]),                                   \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
} while(0)

"




################################################################################
# Save ====
#


macro_typeswitch_numeric <- stri_c(
  macro_typeswitch_decimal_common,
  "\n",
  macro_typeswitch_decimal_careful,
  "\n",
  macro_typeswitch_decimal_simple,
  "\n",
  macro_typeswitch_decimal_special,
  "\n",
  macro_typeswitch_decimal_rel,
  "\n",
  macro_typeswitch_integer_common,
  "\n",
  macro_typeswitch_integer_special,
  "\n"
)

readr::write_file(macro_typeswitch_numeric, "macro_typeswitch_numeric.txt")
