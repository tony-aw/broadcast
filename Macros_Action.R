
macro_action_common <- "

#define MACRO_ACTION_COMMON(NACHECK, NACODE, DOCODE) do {      \\
  if(NACHECK) {                                                   \\
  	  NACODE;                                                       \\
  	}                                                               \\
  	else {                                                          \\
  	  DOCODE;                                                       \\
  	}                                                               \\
} while(0)

"


readr::write_file(macro_action_common, "macro_action_common.txt")


macro_action_special <- "

#define MACRO_ACTION_SPECIAL(RULECHECK, RULECODE, NACHECK, NACODE, DOCODE) do {      \\
  if(RULECHECK) {                                                   \\
    RULECODE;                                                       \\
  }                                                                 \\
  else if(NACHECK) {                                                     \\
  	  NACODE;                                                       \\
  }                                                               \\
	else {                                                          \\
	  DOCODE;                                                       \\
	}                                                               \\
} while(0)

"


readr::write_file(macro_action_special, "macro_action_special.txt")

