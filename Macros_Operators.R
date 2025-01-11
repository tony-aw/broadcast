

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
# Numeric ====
#


macro_op_num_math <- "
#define MACRO_OP_NUM_MATH(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] + (double)py[flatind_y])	\\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] - (double)py[flatind_y])	\\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] * (double)py[flatind_y])	\\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C((double)px[flatind_x] / (double)py[flatind_y])	\\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(	\\
        DIMCODE,	\\
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,	\\
        MACRO_ASSIGN_C(1),	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(R_pow((double)px[flatind_x], (double)py[flatind_y]))	\\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(rcpp_mod_longint((double)px[flatind_x], (double)py[flatind_y])) 	\\
      );	\\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\\
      );	\\
      break;	\\
    }	\\
    case 8:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
        DIMCODE,	\\
        MACRO_ASSIGN_C(NA_REAL),	\\
        MACRO_ASSIGN_C(((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\\
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


macro_op_num_rel <- "
#define MACRO_OP_NUM_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] == py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] != py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] < py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] > py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] <= py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] >= py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 7:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]), \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc < prec)  \\
    );	\\
    break;	\\
  }	\\
  case 8:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]),	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc >= prec)  \\
    );	\\
    break;	\\
  }	\\
  case 9:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc <= -prec)  \\
    );	\\
    break;	\\
  }	\\
  case 10:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc >= prec)  \\
    );	\\
    break;	\\
  }	\\
  case 11:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc < prec)  \\
    );	\\
    break;	\\
  }	\\
  case 12:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(tempcalc > -prec)  \\
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


macro_op_bool_math <- "
#define MACRO_OP_B_ANDOR(DIMCODE) do {	\\
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


macro_op_bool_rel <- "
#define MACRO_OP_B_REL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] == py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] != py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] < py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] > py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      MACRO_ASSIGN_C(NA_LOGICAL), \\
      MACRO_ASSIGN_C(px[flatind_x] <= py[flatind_y])  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
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




macro_op_str_conc <- "
#define MACRO_OP_STR_CONC(DIMCODE) do {	\\
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
# Save Macros ====
#

macro_op <- stri_c(
  macro_assign_C,
  "\n",
  macro_op_num_math,
  "\n",
  macro_op_num_rel,
  "\n",
  macro_op_bool_math,
  "\n",
  macro_op_bool_rel,
  "\n",
  macro_op_str_conc,
  "\n",
  macro_op_str_rel,
  "\n"
)

readr::write_file(macro_op, "macro_op.txt")
