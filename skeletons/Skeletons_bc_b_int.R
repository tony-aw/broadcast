# set-up ====
library(stringi)
source("SOURCE.R")

################################################################################
# Inlines ====
#

inlines <- "

inline int inline_bool_AND(
  int x, int y
) {
  bool xFALSE = x == 0;
  bool yFALSE = y == 0;
  if(xFALSE || yFALSE) {
    return 0;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x && (bool)y);
  }
}

inline int inline_bool_OR(
  int x, int y
) {
  bool xTRUE = x != NA_INTEGER && x != 0;
  bool yTRUE = y != NA_INTEGER && y != 0;
  if(xTRUE || yTRUE) {
    return 1;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x || (bool)y);
  }
}


inline int inline_bool_XOR(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x != bool(y));
  }
}

inline int inline_bool_XNOR(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x == bool(y));
  }
}

inline int inline_bool_NAND(
  int x, int y
) {
  bool xFALSE = x == 0;
  bool yFALSE = y == 0;
  if(xFALSE || yFALSE) {
    return 1;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    int out = !((bool)x && (bool)y);
    return out;
  }
}


inline int inline_bool_NOR(
  int x, int y
) {
  bool xTRUE = x != NA_INTEGER && x != 0;
  bool yTRUE = y != NA_INTEGER && y != 0;
  if(xTRUE || yTRUE) {
    return 0;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    int out = !((bool)x || bool(y));
    return out;
  }
}



"


Rcpp::sourceCpp(code = paste0(header_for_sourcing, inlines, collapse = "\n"))


################################################################################
# MACROs ====
#


macro_acion_bool <- "
#define MACRO_ACTION_BOOL_INT(DIMCODE, DOCODE) do {     \\
  DIMCODE(                                              \\
    pout[flatind_out] = DOCODE,                         \\
    const int tempx = px[flatind_x],                    \\
    const int tempy = py[flatind_y]                     \\
  );                                                    \\
} while(0)

"


macro_op_bool_int <- "
#define MACRO_OP_BOOL_INT(DIMCODE) do {	\\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_AND(tempx, tempy));  \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_OR(tempx, tempy));  \\
      break;	\\
    }	\\
    case 3:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XOR(tempx, tempy));  \\
      break;	\\
    }	\\
    case 4:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_NAND(tempx, tempy));  \\
      break;	\\
    }	\\
    case 5:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_NOR(tempx, tempy));  \\
      break;	\\
    }	\\
    case 6:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XNOR(tempx, tempy));  \\
      break;	\\
    }	\\
    case 7:	\\
    {	\\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XOR(tempx, tempy));  \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"




macros_bool <- paste0(macro_acion_bool, macro_op_bool_int, collapse = "\n")


Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_bool, collapse = "\n"))

################################################################################
# Functions ====
#

prep <- "
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = LOGICAL(out); 
"

txt1 <- generate_functions(
  "bc_b_int", "LogicalVector", prep, "MACRO_OP_BOOL_INT"
)
cat(txt1)



txt <- stringi::stri_c(
  header_for_sourcing,
  inlines,
  macros_bool,
  txt1,
  collapse = "\n\n"
)

Rcpp::sourceCpp(code = txt)


setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  inlines,
  macros_bool,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_b_int.cpp")

