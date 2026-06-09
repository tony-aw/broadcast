library(stringi)


################################################################################
# Introduction ====
#

introcomments <- "

********************************************************************************
INLINE FUNCTIONS

********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)



################################################################################
# Boolean ====
#

inline_bool <- "


inline int inline_bool_AND(
  int x, int y
) {
  bool xFALSE = x != NA_INTEGER && x == 0;
  bool yFALSE = y != NA_INTEGER && y == 0;
  if(xFALSE || yFALSE) {
    return 0;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x && bool(y));
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
    return ((bool)x || bool(y));
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


inline int inline_bool_NAND(
  int x, int y
) {
  bool xFALSE = x != NA_INTEGER && x == 0;
  bool yFALSE = y != NA_INTEGER && y == 0;
  if(xFALSE || yFALSE) {
    return 1;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    int out = ((bool)x + bool(y)) < 2;
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



################################################################################
# Save ====
#

inlines <- stri_c(
  "\n",
  introcomments, 
  "\n",
  inline_bool,
  "\n"
)

readr::write_file(inlines, "inlines.txt")
