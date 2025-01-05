

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

macro_op_rel_dbl <- "
#define MACRO_OP_REL_DBL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] == py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 2:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] != py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 3:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] < py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 4:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] > py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 5:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] <= py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 6:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
      DIMCODE,	\\
      tempout = NA_LOGICAL, \\
      tempout = px[flatind_x] >= py[flatind_y]  \\
    );	\\
    break;	\\
  }	\\
  case 7:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]), \\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc < prec  \\
    );	\\
    break;	\\
  }	\\
  case 8:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = abs((double)px[flatind_x] - (double)py[flatind_y]),	\\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc >= prec  \\
    );	\\
    break;	\\
  }	\\
  case 9:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc <= -prec  \\
    );	\\
    break;	\\
  }	\\
  case 10:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc >= prec  \\
    );	\\
    break;	\\
  }	\\
  case 11:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),  \\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc < prec  \\
    );	\\
    break;	\\
  }	\\
  case 12:	\\
  {	\\
    MACRO_TYPESWITCH_NUMERIC_REL(	\\
      DIMCODE,	\\
      tempcalc = NA_REAL,	\\
      tempcalc = ((double)px[flatind_x] - (double)py[flatind_y]),	\\
      tempout = NA_LOGICAL, \\
      tempout = tempcalc > -prec  \\
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

macro_op_dbl <- "
#define MACRO_OP_DBL(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = (double)px[flatind_x] + (double)py[flatind_y]	\\
      );	\\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = (double)px[flatind_x] - (double)py[flatind_y]	\\
      );	\\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = (double)px[flatind_x] * (double)py[flatind_y]	\\
      );	\\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_COMMON(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = (double)px[flatind_x] / (double)py[flatind_y]	\\
      );	\\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(	\\
        DIMCODE,	\\
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,	\\
        tempout = 1,	\\
        tempout = NA_REAL,	\\
        tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])	\\
      );	\\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 	\\
      );	\\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      MACRO_TYPESWITCH_NUMERIC_CAREFUL(	\\
        DIMCODE,	\\
        tempout = NA_REAL,	\\
        tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 	\\
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
  macro_op_dbl,
  macro_op_rel_dbl
)

readr::write_file(macro_op, "macro_op.txt")
