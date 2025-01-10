
library(stringi)

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

macro_action <- stri_c(
  macro_action1,
  "\n",
  macro_action2,
  "\n",
  macro_action3,
  "\n",
  macro_action4,
  "\n",
  macro_action_boolean,
  "\n",
  macro_doublepass
)

readr::write_file(macro_action, "macro_action.txt")

