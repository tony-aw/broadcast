

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
# Numeric ====
#

macro_assign_C <- "
#define MACRO_ASSIGN_C(INPUTCODE) do {  \\
  tempout = INPUTCODE;              \\
  pout[flatind_out] = tempout;      \\
} while(0)
"

macro_op_dbl <- "
#define MACRO_OP_DBL(DIMCODE) do {	\\
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
        MACRO_ASSIGN_C(((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y]) 	\\
      );	\\
      break;	\\
    }	\\
    case 7:	\\
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


macro_op_rel_dbl <- "
#define MACRO_OP_REL_DBL(DIMCODE) do {	\\
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

macro_op <- stri_c(
  macro_assign_C,
  "\n",
  macro_op_dbl,
  "\n",
  macro_op_rel_dbl,
  "\n"
)

readr::write_file(macro_op, "macro_op.txt")
