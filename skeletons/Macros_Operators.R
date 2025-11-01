library(stringi)

txt <- "
"
txt <- stri_split_regex(
  txt, "\\n", simplify = TRUE
)
txt <- stri_c(txt, "\t\\\\")
txt <- stri_c(txt, collapse = "\n")
cat(txt)



################################################################################
# Introduction ====
#

introcomments <- "


********************************************************************************
MACROs for operations

The following MACROs define the various operations, like relational and arithmetic operations.

********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)



################################################################################
# Assignment ====
#


macro_assign_C <- "
#define MACRO_ASSIGN_C(INPUTCODE) do {  \\
  tempout = INPUTCODE;              \\
  pout[flatind_out] = tempout;      \\
} while(0)
"


macro_assign_Rcpp <- "
#define MACRO_ASSIGN_RCPP(INPUTCODE) do {  \\
  out[flatind_out] = INPUTCODE;      \\
} while(0)
"

################################################################################
# Decimal ====
#


macro_op_dec_math <- "
#define MACRO_OP_DEC_MATH(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_ARITH(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(px[flatind_x] + py[flatind_y]),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y]),  \\
        MACRO_ASSIGN_C(NA_REAL) \\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_ARITH(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(px[flatind_x] - py[flatind_y]),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y]),  \\
        MACRO_ASSIGN_C(NA_REAL) \\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_ARITH(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(px[flatind_x] * py[flatind_y]),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y]),  \\
        MACRO_ASSIGN_C(NA_REAL) \\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_ARITH(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(px[flatind_x] / py[flatind_y]),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] / (double)py[flatind_y]),  \\
        MACRO_ASSIGN_C(NA_REAL) \\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_SPECIAL(	\\
        DIMCODE,	\\
        px[flatind_x] == 1 || py[flatind_y] == 0,	\\
        MACRO_ASSIGN_C(1),	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(R_pow((double)px[flatind_x], (double)py[flatind_y]))	\\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((px[flatind_x] < py[flatind_y]) ? px[flatind_x] : py[flatind_y]) 	\\
      );	\\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((px[flatind_x] > py[flatind_y]) ? px[flatind_x] : py[flatind_y]) 	\\
      );	\\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macro_op_dec_rel <- "
#define MACRO_OP_DEC_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] == py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] != py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] < py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] > py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] <= py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] >= py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


macro_op_dec_dist <- "
#define MACRO_OP_DEC_DIST(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = fabs(px[flatind_x] - py[flatind_y]), \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc <= prec)  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = fabs(px[flatind_x] - py[flatind_y]),	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc > prec)  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = (px[flatind_x] - py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc < -prec)  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = (px[flatind_x] - py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc > prec)  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = (px[flatind_x] - py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc <= prec)  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_DECIMAL_DIST(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = (px[flatind_x] - py[flatind_y]),	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc >= -prec)  \\
    );	\\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


################################################################################
# Integer ====
#

macro_op_int_math <- "
#define MACRO_OP_INT_MATH(DIMCODE) do {	\\
  double intmax = pow(2, 53) - 1;           \\
  double intmin = -1 * intmax;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      if(!rcpp_int53_need_guard1(x, y)) {    \\
        MACRO_TYPESWITCH_INTEGER_UNGUARDED(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y])	\\
        );	\\
      }   \\
      else {    \\
        MACRO_TYPESWITCH_INTEGER1(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 + e2, intmin, intmax))	\\
        );	\\
      }   \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      if(!rcpp_int53_need_guard1(x, y)) {    \\
        MACRO_TYPESWITCH_INTEGER_UNGUARDED(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y])	\\
        );	\\
      }   \\
      else {    \\
        MACRO_TYPESWITCH_INTEGER1(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 - e2, intmin, intmax))	\\
        );	\\
      }   \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      if(!rcpp_int53_need_guard2(x, y)) {    \\
        MACRO_TYPESWITCH_INTEGER_UNGUARDED(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y])	\\
        );	\\
      }   \\
      else {    \\
        MACRO_TYPESWITCH_INTEGER1(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C(rcpp_int53_guard(e1 * e2, intmin, intmax))	\\
        );	\\
      }   \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER2(	\\
        DIMCODE,	\\
        trunc(px[flatind_x]) == 1 || trunc(py[flatind_y]) == 0,	\\
        MACRO_ASSIGN_C(1),	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(rcpp_int53_guard(trunc(R_pow(e1, e2)), intmin, intmax))	\\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER1(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((e1 < e2) ? e1 : e2) 	\\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER1(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((e1 > e2) ? e1 : e2) 	\\
      );	\\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macro_op_int_fact <- "
#define MACRO_OP_INT_FACT(DIMCODE) do {	\\
  double intmax = pow(2, 53) - 1;           \\
  double intmin = -1 * intmax;          \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER_GCD(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((double)rcpp_int53_gcd(px[flatind_x], py[flatind_y]))	\\
      );    \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER1(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C(rcpp_int53_mod(e1, e2, intmin, intmax))	\\
        );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_INTEGER1(	\\
          DIMCODE,	\\
          MACRO_ASSIGN_C(NA_REAL),	\\
          MACRO_ASSIGN_C(rcpp_int53_idiv(e1, e2, intmin, intmax))	\\
        );	\\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macro_op_int_rel <- "
#define MACRO_OP_INT_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 == e2)  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 != e2)  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 < e2)  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 > e2)  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 <= e2)  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_INTEGER1(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(e1 >= e2)  \\
    );	\\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


################################################################################
# Boolean ====
#


macro_op_bool_andor_int <- "
#define MACRO_OP_BOOL_ANDOR_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ACTION_BOOLEAN(       \\
          px[flatind_x], py[flatind_y],       \\
          xFALSE || yFALSE,         \\
          MACRO_ASSIGN_C(0),        \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                 \\
          MACRO_ASSIGN_C((bool)px[flatind_x] && (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                       \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN(                                           \\
          px[flatind_x], py[flatind_y],       \\
          xTRUE || yTRUE,                   \\
          MACRO_ASSIGN_C(1),                                            \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] || (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION2(                                                  \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,   \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN(                                           \\
          px[flatind_x], py[flatind_y],       \\
          xTRUE || yTRUE,                   \\
          MACRO_ASSIGN_C(0),                                            \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C(!(bool)px[flatind_x] && !(bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macro_op_bool_rel_int <- "
#define MACRO_OP_BOOL_REL_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] == (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] < (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] > (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] <= (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ACTION_BOOLEAN_REL(                                           \\
          px[flatind_x], py[flatind_y],       \\
          MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
          MACRO_ASSIGN_C((bool)px[flatind_x] >= (bool)py[flatind_y])  \\
        )                                                       \\
      );                                                        \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macro_op_bool_andor_raw <- "
#define MACRO_OP_BOOL_ANDOR_RAW(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] && (bool)py[flatind_y])  \\
      );                                                       \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                                                          \\\
        MACRO_ASSIGN_C((bool)px[flatind_x] || (bool)py[flatind_y])  \\
      );                                                        \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(                                                          \\\
        MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                                                          \\
        MACRO_ASSIGN_C(!(bool)px[flatind_x] && !(bool)py[flatind_y])  \\
      );                                                        \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] == (bool)py[flatind_y])  \\
      );                                                   \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \\
      );                                                     \\
      break;	\\
    }	\\
    case 7:	\\
    { \\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] < (bool)py[flatind_y])  \\
      );                                                     \\
        break;	\\
    }	\\
    case 8:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] > (bool)py[flatind_y])  \\
      );                                                      \\
      break;	\\
    }	\\
    case 9:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] <= (bool)py[flatind_y])  \\
      );                                                     \\
      break;	\\
    }	\\
    case 10:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] >= (bool)py[flatind_y])  \\
      );                                                    \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"

macro_op_bool_rel_raw <- "
#define MACRO_OP_BOOL_REL_RAW(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] == (bool)py[flatind_y])  \\
      );                                                   \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] != (bool)py[flatind_y])  \\
      );                                                     \\
      break;	\\
    }	\\
    case 3:	\\
    { \\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] < (bool)py[flatind_y])  \\
      );                                                     \\
        break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] > (bool)py[flatind_y])  \\
      );                                                      \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] <= (bool)py[flatind_y])  \\
      );                                                     \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(                      \\
        MACRO_ASSIGN_C((bool)px[flatind_x] >= (bool)py[flatind_y])  \\
      );                                                    \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


################################################################################
# Complex ====
#



macro_op_cplx_rel <- "
#define MACRO_OP_CPLX_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_equal(px[flatind_x], py[flatind_y])  \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_unequal(px[flatind_x], py[flatind_y])  \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"




macro_op_cplx_math <- "
#define MACRO_OP_CPLX_MATH(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_plus(px[flatind_x], py[flatind_y])  \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_min(px[flatind_x], py[flatind_y])  \\
    );                                                                \\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_mult(px[flatind_x], py[flatind_y])  \\
    );                                                                \\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_cplx_div(px[flatind_x], py[flatind_y])   \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"






################################################################################
# String ====
#

macro_op_str_rel <- "
#define MACRO_OP_STR_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                                  \\
        px[flatind_x] == NA_STRING || py[flatind_y] == NA_STRING,   \\
        MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
        MACRO_ASSIGN_C((int)R_compute_identical(px[flatind_x], py[flatind_y], 0))  \\
      )                                                       \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      MACRO_ACTION2(                                                  \\
        px[flatind_x] == NA_STRING || py[flatind_y] == NA_STRING,   \\
        MACRO_ASSIGN_C(NA_LOGICAL),                                   \\
        MACRO_ASSIGN_C((int)!R_compute_identical(px[flatind_x], py[flatind_y], 0))  \\
      )                                                       \\
    );         \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


macro_op_str_dist <- "
#define MACRO_OP_STR_DIST(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = rcpp_str_dist_led(px[flatind_x], py[flatind_y])   \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"



macro_op_str_plus <- "
#define MACRO_OP_STR_PLUS(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      out[flatind_out] = rcpp_string_plus(px[flatind_x], py[flatind_y]) \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"




################################################################################
# Raw ====
#


macro_op_raw_rel <- "
#define MACRO_OP_RAW_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] == py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] != py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] < py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] > py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] <= py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    DIMCODE(                                                          \\
      pout[flatind_out] = px[flatind_x] >= py[flatind_y]                \\
    );                                                                \\
    break;	\\
  }	\\
  default:	\\
  {	\\
    stop(\"given operator not supported in the given context\");	\\
  }	\\
}	\\
} while(0)
"


macro_op_raw_byte <- "
#define MACRO_OP_RAW_BYTE(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] == py[flatind_y]                \\
      );                                                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] != py[flatind_y]                \\
      );                                                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] < py[flatind_y]                \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] > py[flatind_y]                \\
      );                                                                \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] <= py[flatind_y]                \\
      );                                                                \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = px[flatind_x] >= py[flatind_y]                \\
      );                                                                \\
      break;	\\
    } \\
    case 7:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = (px[flatind_x] < py[flatind_y]) ? px[flatind_x] : py[flatind_y]               \\
      );                                                                \\
      break;	\\
    }	\\
    case 8:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = (px[flatind_x] > py[flatind_y]) ? px[flatind_x] : py[flatind_y]               \\
      );                                                                \\
      break;	\\
    }	\\
    case 9:	\\
    {	\\
      DIMCODE(                                                          \\
        pout[flatind_out] = rcpp_raw_diff(px[flatind_x], py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"




################################################################################
# Bit-wise ====
#

macro_op_bit_andor_raw <- "
#define MACRO_OP_BIT_ANDOR_RAW(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = px[flatind_x] & py[flatind_y] \\
      );                                                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = px[flatind_x] | py[flatind_y] \\
      );                                                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = px[flatind_x] ^ py[flatind_y] \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = (~px[flatind_x] & ~py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = rcpp_bit_ls(px[flatind_x], py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = rcpp_bit_rs(px[flatind_x], py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"



macro_op_bit_rel_raw <- "
#define MACRO_OP_BIT_REL_RAW(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = (px[flatind_x] & py[flatind_y]) | (~px[flatind_x] & ~py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = px[flatind_x] ^ py[flatind_y] \\
      );                                                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = (~px[flatind_x] & py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = (px[flatind_x] & ~py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = rcpp_bit_se_raw(px[flatind_x], py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      DIMCODE(  \\
        pout[flatind_out] = rcpp_bit_ge_raw(px[flatind_x], py[flatind_y]) \\
      );                                                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"

macro_op_bit_andor_int <- "
#define MACRO_OP_BIT_ANDOR_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = px[flatind_x] & py[flatind_y]                \\
        ) \\
      );                                                                \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = px[flatind_x] | py[flatind_y]                \\
        ) \\
      );                                                                \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = px[flatind_x] ^ py[flatind_y]                \\
        ) \\
      );                                                                \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = (~px[flatind_x]) & (~py[flatind_y])         \\
        ) \\
      );                                                                \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"


macro_op_bit_rel_int <- "
#define MACRO_OP_BIT_REL_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = (px[flatind_x] & py[flatind_y]) | (~px[flatind_x] & ~py[flatind_y]) \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    case 2:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = px[flatind_x] ^ py[flatind_y] \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    case 3:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = (~px[flatind_x] & py[flatind_y]) \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    case 4:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = (px[flatind_x] & ~py[flatind_y]) \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    case 5:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = rcpp_bit_se_int(px[flatind_x], py[flatind_y]) \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    case 6:	\\
    {	\\
      DIMCODE(  \\
        MACRO_ACTION2(                                                    \\
          px[flatind_x] == NA_INTEGER || py[flatind_y] == NA_INTEGER,     \\
          pout[flatind_out] = NA_INTEGER,                                                     \\
          pout[flatind_out] = rcpp_bit_ge_int(px[flatind_x], py[flatind_y]) \\
        ) \\
      );                                                                \\
      break;  \\
    } \\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  } \\
} while(0)
"



################################################################################
# Ifelse ====

macro_op_ifelse_int <- "
#define MACRO_OP_IFELSE_INT(DIMCODE) do {       \\
  switch(TYPEOF(x)) {	\\
    case LGLSXP:	\\
    case INTSXP:	\\
    {	\\
      const int *px = INTEGER_RO(x);	\\
      const int *py = INTEGER_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(TYPEOF(x), nout));	\\
      int *pout;	\\
      pout = INTEGER(out);	\\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          pout[flatind_out] = NA_INTEGER,	\\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case REALSXP:	\\
    {	\\
      const double *px = REAL_RO(x);	\\
      const double *py = REAL_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));	\\
      double *pout;	\\
      pout = REAL(out);	\\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          pout[flatind_out] = NA_REAL,	\\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case CPLXSXP:	\\
    {	\\
      const Rcomplex *px = COMPLEX_RO(x);	\\
      const Rcomplex *py = COMPLEX_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));	\\
      Rcomplex *pout;	\\
      pout = COMPLEX(out);	\\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          pout[flatind_out] = rcpp_cplx_returnNA(),	\\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case STRSXP:	\\
    {	\\
      const SEXP *px = STRING_PTR_RO(x);	\\
      const SEXP *py = STRING_PTR_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(STRSXP, nout));	\\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          SET_STRING_ELT(out, flatind_out, NA_STRING),	\\
          SET_STRING_ELT(out, flatind_out, pcond[flatind_out] ? px[flatind_x] : py[flatind_y])	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case RAWSXP:	\\
    {	\\
      const Rbyte *px = RAW_RO(x);	\\
      const Rbyte *py = RAW_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));	\\
      Rbyte *pout = RAW(out); \\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          stop(\"NA condition not supported for type of `raw`\"),	\\
          pout[flatind_out] = pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case VECSXP:	\\
    {	\\
      SEXP out = PROTECT(Rf_allocVector(VECSXP, nout));	\\
      	\\
      DIMCODE(	\\
        MACRO_ACTION2(	\\
          pcond[flatind_out] == NA_INTEGER,	\\
          SET_VECTOR_ELT(out, flatind_out, R_NilValue),	\\
          SET_VECTOR_ELT(out, flatind_out, pcond[flatind_out] ? VECTOR_ELT(x, flatind_x) : VECTOR_ELT(y, flatind_y))	\\
        )	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    default:	\\
    {	\\
      stop(\"unsupported type\");	\\
    }	\\
  }	\\
} while(0)
"

macro_op_ifelse_raw <- "
#define MACRO_OP_IFELSE_RAW(DIMCODE) do {       \\
  switch(TYPEOF(x)) {	\\
    case LGLSXP:	\\
    {	\\
      const int *px = LOGICAL_RO(x);	\\
      const int *py = LOGICAL_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(LGLSXP, nout));	\\
      int *pout;	\\
      pout = LOGICAL(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case INTSXP:	\\
    {	\\
      const int *px = INTEGER_RO(x);	\\
      const int *py = INTEGER_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(INTSXP, nout));	\\
      int *pout;	\\
      pout = INTEGER(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case REALSXP:	\\
    {	\\
      const double *px = REAL_RO(x);	\\
      const double *py = REAL_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));	\\
      double *pout;	\\
      pout = REAL(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case CPLXSXP:	\\
    {	\\
      const Rcomplex *px = COMPLEX_RO(x);	\\
      const Rcomplex *py = COMPLEX_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(CPLXSXP, nout));	\\
      Rcomplex *pout;	\\
      pout = COMPLEX(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case STRSXP:	\\
    {	\\
      const SEXP *px = STRING_PTR_RO(x);	\\
      const SEXP *py = STRING_PTR_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(STRSXP, nout));	\\
      	\\
      DIMCODE(	\\
        SET_STRING_ELT(out, flatind_out, (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y])	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case RAWSXP:	\\
    {	\\
      const Rbyte *px = RAW_RO(x);	\\
      const Rbyte *py = RAW_RO(y);	\\
      	\\
      SEXP out = PROTECT(Rf_allocVector(RAWSXP, nout));	\\
      Rbyte *pout = RAW(out); \\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = (bool)pcond[flatind_out] ? px[flatind_x] : py[flatind_y]	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    case VECSXP:	\\
    {	\\
      SEXP out = PROTECT(Rf_allocVector(VECSXP, nout));	\\
      	                                                \\
      DIMCODE(	\\
        SET_VECTOR_ELT(out, flatind_out, (bool)pcond[flatind_out] ? VECTOR_ELT(x, flatind_x) : VECTOR_ELT(y, flatind_y))	\\
      );	\\
      	\\
      UNPROTECT(1);	\\
      return out;	\\
      	\\
    }	\\
    default:	\\
    {	\\
      stop(\"unsupported type\");	\\
    }	\\
  }	\\
} while(0)
"


################################################################################
# bcapply ====

macro_op_bcapply <- "
#define MACRO_OP_BCAPPLY(DIMCODE) do {       \\
  switch(TYPEOF(out)) {	\\
    case LGLSXP:	\\
    {	\\
      int *pout;	\\
      pout = LOGICAL(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = LOGICAL(f(x, y, flatind_x + 1, flatind_y + 1))[0] \\
      );	\\
      break; \\
    }	\\
    case INTSXP:	\\
    {	\\
      int *pout;	\\
      pout = INTEGER(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = INTEGER(f(x, y, flatind_x + 1, flatind_y + 1))[0] \\
      );	\\
      break; \\
    }	\\
    case REALSXP:	\\
    {	\\
      double *pout;	\\
      pout = REAL(out);	\\
      DIMCODE(	\\
        pout[flatind_out] = REAL(f(x, y, flatind_x + 1, flatind_y + 1))[0] \\
      );	\\
      break; \\
    }	\\
    case CPLXSXP:	\\
    {	\\
      Rcomplex *pout;	\\
      pout = COMPLEX(out);	\\
      DIMCODE(	\\
        pout[flatind_out] = COMPLEX(f(x, y, flatind_x + 1, flatind_y + 1))[0] \\
      );	\\
      break; \\
    }	\\
    case STRSXP:	\\
    {	\\
      	\\
      DIMCODE(	\\
        SET_STRING_ELT(out, flatind_out, STRING_ELT(f(x, y, flatind_x + 1, flatind_y + 1), 0)) \\
      );	\\
      break; \\
    }	\\
    case RAWSXP:	\\
    {	\\
      Rbyte *pout;	\\
      pout = RAW(out);	\\
      	\\
      DIMCODE(	\\
        pout[flatind_out] = RAW(f(x, y, flatind_x + 1, flatind_y + 1))[0] \\
      );	\\
      break; \\
    }	\\
    case VECSXP:	\\
    {	\\
     DIMCODE(	\\
        SET_VECTOR_ELT(out, flatind_out, f(x, y, flatind_x + 1, flatind_y + 1))	\\
      );	\\
      break; \\
    }	\\
    default:	\\
    {	\\
      stop(\"unsupported type\");	\\
    }	\\
  }	\\
} while(0)
"


################################################################################
# Save Macros ====
#

macro_op <- stri_c(
  "\n",
  introcomments,
  "\n",
  macro_assign_C,
  "\n",
  macro_op_dec_math,
  "\n",
  macro_op_dec_rel,
  "\n",
  macro_op_dec_dist,
  "\n",
  macro_op_int_math,
  "\n",
  macro_op_int_rel,
  "\n",
  macro_op_int_fact,
  "\n",
  macro_op_bool_andor_int,
  "\n",
  macro_op_bool_andor_raw,
  "\n",
  macro_op_bool_rel_int,
  "\n",
  macro_op_bool_rel_raw,
  "\n",
  macro_op_cplx_math,
  "\n",
  macro_op_cplx_rel,
  "\n",
  macro_op_str_plus,
  "\n",
  macro_op_str_rel,
  "\n",
  macro_op_str_dist,
  "\n",
  macro_op_raw_rel,
  "\n",
  macro_op_raw_byte,
  "\n",
  macro_op_bit_andor_int,
  "\n",
  macro_op_bit_andor_raw,
  "\n",
  macro_op_bit_rel_int,
  "\n",
  macro_op_bit_rel_raw,
  "\n",
  macro_op_ifelse_int,
  "\n",
  macro_op_ifelse_raw,
  "\n",
  macro_op_bcapply,
  "\n"
)

readr::write_file(macro_op, "macro_op.txt")