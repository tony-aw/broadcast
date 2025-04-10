library(stringi)


################################################################################
# Introduction ====
#

introcomments <- "

********************************************************************************
MACROs for \"actions\"

The following MACROs define small pieces of code that are used at various places in other MACROs.

********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)



################################################################################
# General ====
#

macro_action1 <- "

#define MACRO_ACTION1(DOCODE) do {      \\
  DOCODE;                                     \\
} while(0)

"



macro_action2 <- "

#define MACRO_ACTION2(NACHECK, NACODE, DOCODE) do {      \\
  if(NACHECK) {                                                   \\
  	  NACODE;                                                       \\
  	}                                                               \\
  	else {                                                          \\
  	  DOCODE;                                                       \\
  	}                                                               \\
} while(0)

"



macro_action3 <- "

#define MACRO_ACTION3(RULECHECK, RULECODE, DOCODE) do {      \\
  if(RULECHECK) {                                                   \\
    RULECODE;                                                       \\
  }                                                                 \\
	else {                                                          \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"



macro_action4 <- "

#define MACRO_ACTION4(RULECHECK, RULECODE, NACHECK, NACODE, DOCODE) do {      \\
  if(RULECHECK) {                                                   \\
    RULECODE;                                                       \\
  }                                                                 \\
  else if(NACHECK) {                                                \\
  	  NACODE;                                                       \\
  }                                                               \\
	else {                                                          \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"

macro_doublepass <- "
#define MACRO_DOUBLEPASS(MACRO1, MACRO2) do{  \\
  MACRO1;                                     \\
  MACRO2;                                     \\
} while(0)
"


################################################################################
# Integer ====
#


macro_overflow <- "

#define MACRO_OVERFLOW(REF) ((REF) < intmin || (REF) > intmax)

"

macro_action_integer1 <- "

#define MACRO_ACTION_INTEGER1(NACHECK, NACODE, DOLEFT, DORIGHT, DOCODE) do {      \\
  if(NACHECK) {                                                   \\
  	  NACODE;                                                     \\
  }                                                               \\
	else {                                                          \\
	  e1 = DOLEFT;                                                  \\
	  e2 = DORIGHT;                                                 \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"



macro_action_integer2 <- "

#define MACRO_ACTION_INTEGER2(RULECHECK, RULECODE, NACHECK, NACODE, DOLEFT, DORIGHT, DOCODE) do {      \\
  if(RULECHECK) {                                                   \\
    RULECODE;                                                       \\
  }                                                                 \\
  else if(NACHECK) {                                                   \\
  	  NACODE;                                                     \\
  }                                                               \\
	else {                                                          \\
	  e1 = DOLEFT;                                                  \\
	  e2 = DORIGHT;                                                 \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"



macro_action_integer_gcd1 <- "

#define MACRO_ACTION_INTEGER_GCD1(NACHECK, NACODE, DOCODE) do {      \\
  if(NACHECK) {                                                   \\
    NACODE;                                                     \\
  }                                                             \\
	else {                                                          \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"

macro_action_integer_gcd2 <- "

#define MACRO_ACTION_INTEGER_GCD2(NACHECK, RULECHECK, NACODE, RULECODE, DOCODE) do {      \\
  if(NACHECK) {                                                   \\
    NACODE;                                                     \\
  }                                                                 \\
  else if(RULECHECK) {                                                   \\
    RULECODE;                                                       \\
  }                                                                 \\
	else {                                                          \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"



################################################################################
# Boolean ====
#



macro_action_boolean <- "
#define MACRO_ACTION_BOOLEAN(XREF, YREF, PRECHECK, PRECODE, NACODE, DOCODE) do { \\
                                          \\
  xTRUE = rcpp_isTRUE(XREF);                 \\
  xFALSE = rcpp_isFALSE(XREF);               \\
  xNA = XREF == NA_INTEGER;                  \\
  yTRUE = rcpp_isTRUE(YREF);                 \\
  yFALSE = rcpp_isFALSE(YREF);               \\
  yNA = YREF == NA_INTEGER;                  \\
  if(PRECHECK) {                          \\
    PRECODE;                              \\
  }                                       \\
  else if(xNA || yNA) {                   \\
    NACODE;                               \\
  }                                       \\
  else {                                  \\
    DOCODE;                               \\
  }                                       \\
} while(0)

"

macro_action_boolean_rel <- "
#define MACRO_ACTION_BOOLEAN_REL(XREF, YREF, NACODE, DOCODE) do { \\
  if(XREF == NA_INTEGER || YREF == NA_INTEGER) {                   \\
    NACODE;                               \\
  }                                       \\
  else {                                  \\
    DOCODE;                               \\
  }                                       \\
} while(0)

"



################################################################################
# Generic Method  ====
#

macro_action_vapply <- "
#define MACRO_ACTION_VAPPLY(ASSIGNCODE, ERRORCHECK, ERRORCODE, DOCODE) do { \\
  ASSIGNCODE;   \\
  if(ERRORCHECK) {  \\
    ERRORCODE;  \\
  } \\
  else {  \\
    DOCODE; \\
  } \\
} while(0)
"




################################################################################
# Save ====
#

macro_action <- stri_c(
  "\n",
  introcomments, 
  "\n",
  macro_overflow,
  "\n",
  macro_action1,
  "\n",
  macro_action2,
  "\n",
  macro_action3,
  "\n",
  macro_action4,
  "\n",
  macro_action_integer1,
  "\n",
  macro_action_integer2,
  "\n",
  macro_action_integer_gcd1,
  "\n",
  macro_action_integer_gcd2,
  "\n",
  macro_action_boolean,
  "\n",
  macro_action_boolean_rel,
  "\n",
  macro_doublepass,
  "\n",
  macro_action_vapply,
  "\n"
)

readr::write_file(macro_action, "macro_action.txt")
