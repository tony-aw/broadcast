
library(stringi)


################################################################################
# Introduction ====
#

introcomments <- "


********************************************************************************
MACROs for numeric type-switching

There are several numeric-like types (exclusing complex type):
logical, integer, and double.
The following MACROs define various if-else constructs,
used to decide what specific code should run for which numeric-like type.

********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)


################################################################################
# Decimal ====
#

macro_typeswitch_decimal_arith <- "

#define MACRO_TYPESWITCH_DECIMAL_ARITH(DIMCODE, DECIMALCODE, INTEGERCODE, NACODE) do {      \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {  \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(            \\
        MACRO_ACTION1(    \\
          DECIMALCODE    \\
        )   \\
      );    \\
    }   \\
    else {  \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(            \\
        MACRO_ACTION2(                                           \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
          NACODE,                                               \\
          INTEGERCODE                                                \\
        )                                                       \\
      );    \\
    } \\
} while(0)

"



macro_typeswitch_decimal_careful <- "

#define MACRO_TYPESWITCH_DECIMAL_CAREFUL(DIMCODE, NACODE, DOCODE) do {      \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {    \\
      const double *px = REAL_RO(x);                                           \\
      const double *py = REAL_RO(y);                                           \\
      DIMCODE(                                                          \\
        MACRO_ACTION2(                                           \\
          R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
          NACODE,                                               \\
          DOCODE                                                \\
        )                                                       \\
      );                                                        \\
    }   \\
    else {    \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                                                          \\
        MACRO_ACTION2(                                           \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
          NACODE,                                               \\
          DOCODE                                                \\
        )                                                       \\
      );    \\
    }   \\
} while(0)

"




macro_typeswitch_decimal_special <- "

#define MACRO_TYPESWITCH_DECIMAL_SPECIAL(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \\
  if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {                                 \\
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
  else {                                        \\
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
} while(0)

"



macro_typeswitch_decimal_dist <- "

#define MACRO_TYPESWITCH_DECIMAL_DIST(DIMCODE, NACODE1, DOCODE1, NACODE2, DOCODE2) do {      \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {  \\
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
    } \\
    else {  \\
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
    }   \\
} while(0)

"




################################################################################
# Integer ====
#



macro_typeswitch_integer_unguarded <- "

#define MACRO_TYPESWITCH_INTEGER_UNGUARDED(DIMCODE, NACODE, DOCODE) do {      \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                           \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
} while(0)

"


macro_typeswitch_integer_common <- "

#define MACRO_TYPESWITCH_INTEGER_COMMON(DIMCODE, NACODE, DOCODE) do {      \\
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

#define MACRO_TYPESWITCH_INTEGER2(DIMCODE, RULECHECK, RULECODE, NACODE, DOCODE) do {      \\
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


macro_typeswitch_integer_gcd <- "

#define MACRO_TYPESWITCH_INTEGER_GCD(DIMCODE, NACODE, RULECODE, DOCODE) do {      \\
  bool xint = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;   \\
  bool yint = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;   \\
  if(xint && yint) {                                        \\
    const int *px = INTEGER_RO(x);                                        \\
    const int *py = INTEGER_RO(y);                                        \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER_GCD1(                                           \\
        px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,  \\
        NACODE,                                               \\
        DOCODE                                                \\
      )                                                       \\
    );                                                       \\
  }                                                         \\
  else if(!xint && !yint) {                                 \\
    const double *px = REAL_RO(x);                                           \\
    const double *py = REAL_RO(y);                                           \\
    DIMCODE(                                                          \\
      MACRO_ACTION_INTEGER_GCD2(                                           \\
        R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y]),  \\
        MACRO_OVERFLOW(px[flatind_x]) || MACRO_OVERFLOW(py[flatind_y]),           \\
        NACODE,                                               \\
        RULECODE,                                               \\
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
  "\n",
  introcomments,
  "\n",
  macro_typeswitch_decimal_arith,
  "\n",
  macro_typeswitch_decimal_careful,
  "\n",
  macro_typeswitch_decimal_special,
  "\n",
  macro_typeswitch_decimal_dist,
  "\n",
  macro_typeswitch_integer_unguarded,
  "\n",
  macro_typeswitch_integer_common,
  "\n",
  macro_typeswitch_integer1,
  "\n",
  macro_typeswitch_integer2,
  "\n",
  macro_typeswitch_integer_gcd,
  "\n"
)

readr::write_file(macro_typeswitch_numeric, "macro_typeswitch_numeric.txt")
