

library(stringi)

################################################################################
# Numeric ====
#

macro_op_rel_dbl <- "
#define MACRO_OP_REL_DBL(DIMCODE) do {	\\
  switch(op) {	\\
  case 1:	\\
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
  case 2:	\\
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
  case 3:	\\
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
  case 4:	\\
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
  case 5:	\\
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
  case 6:	\\
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
# 
# macro_op_rel_dbl <- stri_split_regex(
#   macro_op_rel_dbl, "\\n", simplify = TRUE
# )
# macro_op_rel_dbl <- stri_c(macro_op_rel_dbl, "\t\\\\")
# macro_op_rel_dbl <- stri_c(macro_op_rel_dbl, collapse = "\n")
# cat(macro_op_rel_dbl)

readr::write_file(macro_op_rel_dbl, "macro_op.txt")
