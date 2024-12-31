

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

readr::write_file(macro_action_special, "macro_action_special.txt")

