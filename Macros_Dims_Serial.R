# set-up ====

library(stringi)



################################################################################
# Vector ====
#

macro_dim_vector <- "

#define MACRO_DIM_VECTOR(DOCODE) do {                               \\
  R_xlen_t flatind_x = 0;                                           \\
  R_xlen_t flatind_y = 0;                                           \\
  int by_x = 0;                                                     \\
  int by_y = 0;                                                     \\
  if(Rf_xlength(x) == Rf_xlength(y)) {                              \\
    if(Rf_xlength(x) == 1) {                                        \\
      by_x = 0;                                                     \\
      by_y = 0;                                                     \\
    }                                                               \\
    else {                                                          \\
      by_x = 1;                                                     \\
      by_y = 1;                                                     \\
    }                                                               \\
  }                                                                 \\
  if(Rf_xlength(x) != Rf_xlength(y)) {                              \\
    if(Rf_xlength(x) == 1) {                                        \\
      by_x = 0;                                                     \\
      by_y = 1;                                                     \\
    }                                                               \\
    else if(Rf_xlength(y) ==1) {                                    \\
      by_x = 1;                                                     \\
      by_y = 0;                                                     \\
    }                                                               \\
    else {                                                          \\
      stop(\"unequal length\");                                     \\
    }                                                               \\
  }                                                                 \\
  for(R_xlen_t i = 0; i < nout; ++i) {                              \\
      DOCODE;                                                       \\
  	                                                                \\
  	  pout[i] = tempout;                                            \\
      flatind_x = flatind_x + by_x;                                 \\
      flatind_y = flatind_y + by_y;                                 \\
                                                                    \\
    }                                                               \\
                                                                    \\
} while(0)

"


################################################################################
# BigX ====
#

DTYPES <- c(2:16)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 1:16, 1:16, 0:15, 1:16)
)
fory <- c(
  "\t\\",
  sprintf("i_y%d = pby_y[%d] * iter%d * pdcp_y[%d];\t\\", 2:16, 1:15, 2:16, 1:15)
)
all_for <- stri_c(all_for, fory, sep = "\n")
cat(all_for[16])

all_parts_y <- c(
  "iter1 * pby_y[0]",
  sprintf("i_y%d", 2:16)
)

all_y_decl <- sprintf("i_y%d", 2:16) 

temp <- "

#define MACRO_DIM_BIGX_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pby_y = INTEGER_RO(by_y);        \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x = 0;       \\
  R_xlen_t flatind_y;       \\
  R_xlen_t <all_y_decl>; \\
  <startfor>
        flatind_y = <main_y>;     \\
                                  \\
        DOCODE;                   \\
  	                              \\
        pout[counter] = tempout;        \\
        flatind_x++;                    \\
        counter++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_y_decl <- stri_c(all_y_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_y_decl>",
    "<startfor>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_y_decl,
    current_for,
    current_main_y,
    current_end
  )
  
  out <- stri_replace_all(
    temp,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  dMacro_skeletons[counter] <- out
  counter <- counter + 1
}

cat(dMacro_skeletons[[2]])


macro_dim_bigx <- stri_c(dMacro_skeletons, collapse = "\n")



################################################################################
# BigY ====
#

DTYPES <- c(2:16)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 1:16, 1:16, 0:15, 1:16)
)
forx <- c(
  "\t\\",
  sprintf("i_x%d = pby_x[%d] * iter%d * pdcp_x[%d];\t\\", 2:16, 1:15, 2:16, 1:15)
)
all_for <- stri_c(all_for, forx, sep = "\n")
cat(all_for[16])

all_parts_x <- c(
  "iter1 * pby_x[0]",
  sprintf("i_x%d", 2:16)
)

all_x_decl <- sprintf("i_x%d", 2:16) 

temp <- "

#define MACRO_DIM_BIGY_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pby_x = INTEGER_RO(by_x);        \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y = 0;       \\
  R_xlen_t <all_x_decl>; \\
  <startfor>
        flatind_x = <main_x>;     \\
                                  \\
        DOCODE;                   \\
  	                              \\
        pout[counter] = tempout;        \\
        flatind_y++;                    \\
        counter++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_x_decl>",
    "<startfor>",
    "<main_x>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_x_decl,
    current_for,
    current_main_x,
    current_end
  )
  
  out <- stri_replace_all(
    temp,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  dMacro_skeletons[counter] <- out
  counter <- counter + 1
}

cat(dMacro_skeletons[[2]])


macro_dim_bigy <- stri_c(dMacro_skeletons, collapse = "\n")



################################################################################
# DoCall BigSmall skeleton ====
#


# cases:
case_bigx <-
  "case %d:                                       \\
  MACRO_DIM_BIGX_%d(DOCODE);    \\
  break;                                        \\
"
cases_bigx <- sprintf(case_bigx, 2:16, 2:16) |> stringi::stri_c(collapse = "")

case_bigy <-
  "case %d:                                       \\
  MACRO_DIM_BIGY_%d(DOCODE);    \\
  break;                                        \\
"
cases_bigy <- sprintf(case_bigy, 2:16, 2:16) |> stringi::stri_c(collapse = "")


cat(cases_bigx)
cat(cases_bigy)

templatecode_docall <- "

#define MACRO_DIM_BIGSMALL_DOCALL(DOCODE) do {     \\
  int ndims = Rf_length(out_dim);         \\
                                          \\
  if(bigx) {                           \\
    switch(ndims) {                       \\
      <cases_bigx>                     \\
    }                                     \\
  }                                       \\
  else {                                  \\
    switch(ndims) {                       \\
      <cases_bigy>                     \\
    }                                     \\
  }                                       \\
} while(0)"

templatecode_docall2 <- stringi::stri_replace_all(
  templatecode_docall,
  fixed = c("<cases_bigx>", "<cases_bigy>"),
  replacement = c(cases_bigx, cases_bigy),
  vectorize_all = FALSE
)


cat(templatecode_docall2)


macro_dim_bigsmall_docall <- templatecode_docall2





################################################################################
# General ====
#

DTYPES <- c(2:16)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 1:16, 1:16, 0:15, 1:16)
)
forx <- c(
  "\t\\",
  sprintf("i_x%d = pby_x[%d] * iter%d * pdcp_x[%d];\t\\", 2:16, 1:15, 2:16, 1:15)
)
fory <- c(
  "\t\\",
  sprintf("i_y%d = pby_y[%d] * iter%d * pdcp_y[%d];\t\\", 2:16, 1:15, 2:16, 1:15)
)
all_for <- stri_c(all_for, forx, fory, sep = "\n")
cat(all_for[16])

all_parts_x <- c(
  "iter1 * pby_x[0]",
  sprintf("i_x%d", 2:16)
)
all_parts_y <- c(
  "iter1 * pby_y[0]",
  sprintf("i_y%d", 2:16)
)

all_x_decl <- sprintf("i_x%d", 2:16)
all_y_decl <- sprintf("i_y%d", 2:16) 

temp <- "

#define MACRO_DIM_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pby_x = INTEGER_RO(by_x);        \\
  const int *pby_y = INTEGER_RO(by_y);        \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y;       \\
  R_xlen_t <all_x_decl>; \\
  R_xlen_t <all_y_decl>; \\
  <startfor>
        flatind_x = <main_x>;       \\
        flatind_y = <main_y>;     \\
                                                                    \\
        DOCODE;                                                          \\
  	                                                                \\
        pout[counter] = tempout;        \\
        counter++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_y_decl <- stri_c(all_y_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_x_decl>",
    "<all_y_decl>",
    "<startfor>",
    "<main_x>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_x_decl,
    current_y_decl,
    current_for,
    current_main_x,
    current_main_y,
    current_end
  )
  
  out <- stri_replace_all(
    temp,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  dMacro_skeletons[counter] <- out
  counter <- counter + 1
}

cat(dMacro_skeletons[[2]])


macro_dim_d <- stri_c(dMacro_skeletons, collapse = "\n")



################################################################################
# do call Macro skeleton ====
#


# cases:
case <-
"case %d:                                       \\
  MACRO_DIM_%d(DOCODE);    \\
  break;                                        \\
"
cases <- sprintf(case, 2:16, 2:16) |> stringi::stri_c(collapse = "")


cat(cases)

templatecode_docall <- "

#define MACRO_DIM_DOCALL(DOCODE) do {     \\
  int ndims = Rf_length(out_dim);         \\
                                          \\
  switch(ndims) {       \\
    <cases>       \\
  }       \\
} while(0)"

templatecode_docall2 <- stringi::stri_replace_all(
  templatecode_docall,
  fixed = c("<cases>"),
  replacement = c(cases),
  vectorize_all = FALSE
)


cat(templatecode_docall2)


macro_dim_docall <- templatecode_docall2



################################################################################
# Save macros ====
#

macro_dim <- stri_c(
  "\n",
  macro_dim_vector,
  "\n",
  macro_dim_bigx,
  "\n",
  macro_dim_bigy,
  "\n",
  macro_dim_bigsmall_docall,
  "\n",
  macro_dim_d,
  "\n",
  macro_dim_docall,
  "\n"
)

readr::write_file(macro_dim, "macro_dim.txt")
