# set-up ====

library(stringi)

DTYPES <- seq(2, 16, 2)


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
  for(R_xlen_t flatind_out = 0; flatind_out < nout; ++flatind_out) {  \\
      DOCODE;                                                       \\
      flatind_x = flatind_x + by_x;                                 \\
      flatind_y = flatind_y + by_y;                                 \\
                                                                    \\
    }                                                               \\
                                                                    \\
} while(0)

"



################################################################################
# Orthogonal Vectors (i.e. row vector by column vector or vice-versa) ====
#

macro_dim_orthovector <- "

#define MACRO_DIM_ORTHOVECTOR(DOCODE) do {      \\
  R_xlen_t flatind_out = 0;         \\
  const int N1 = INTEGER(out_dim)[0];      \\
  const int N2 = INTEGER(out_dim)[1];       \\
  if(RxC) { \\
    for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {	\\
  	  for(int flatind_x = 0; flatind_x < N1; ++flatind_x) {	\\
        DOCODE;                         \\
        flatind_out++;                      \\
    	 }	\\
  	 }	\\
  } \\
  else {  \\
    for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {	\\
    	  for(int flatind_y = 0; flatind_y < N1; ++flatind_y) {	\\
          DOCODE;                         \\
          flatind_out++;                      \\
        }	\\
    }	\\
  } \\
} while(0)

"



################################################################################
# BigX ====
#



all_N_decl <- sprintf("const int N%d = INTEGER(out_dim)[%d];\t\\", 1:16, 0:15)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
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
  R_xlen_t flatind_out = 0;         \\
  const int *pby_y = INTEGER_RO(by_y);        \\
  <all_N_decl>
  const double *pdcp_y = REAL_RO(dcp_y);        \\
  R_xlen_t flatind_x = 0;       \\
  R_xlen_t flatind_y;       \\
  R_xlen_t <all_y_decl>; \\
  <startfor>
        flatind_y = <main_y>;     \\
                                  \\
        DOCODE;                   \\
  	                              \\
        flatind_x++;                    \\
        flatind_out++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_N_decl <- stri_c(all_N_decl[1:i], collapse = "\n")
  current_y_decl <- stri_c(all_y_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_N_decl>",
    "<all_y_decl>",
    "<startfor>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_N_decl,
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



all_N_decl <- sprintf("const int N%d = INTEGER(out_dim)[%d];\t\\", 1:16, 0:15)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
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
  R_xlen_t flatind_out = 0;         \\
  const int *pby_x = INTEGER_RO(by_x);        \\
  <all_N_decl>
  const double *pdcp_x = REAL_RO(dcp_x);        \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y = 0;       \\
  R_xlen_t <all_x_decl>; \\
  <startfor>
        flatind_x = <main_x>;     \\
                                  \\
        DOCODE;                   \\
  	                              \\
        flatind_y++;                    \\
        flatind_out++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_N_decl <- stri_c(all_N_decl[1:i], collapse = "\n")
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_N_decl>",
    "<all_x_decl>",
    "<startfor>",
    "<main_x>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_N_decl,
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
cases_bigx <- sprintf(case_bigx, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")

case_bigy <-
  "case %d:                                       \\
  MACRO_DIM_BIGY_%d(DOCODE);    \\
  break;                                        \\
"
cases_bigy <- sprintf(case_bigy, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")


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

all_N_decl <- sprintf("const int N%d = INTEGER(out_dim)[%d];\t\\", 1:16, 0:15)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d <N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
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
  "pby_x[0] * iter1",
  sprintf("i_x%d", 2:16)
)
all_parts_y <- c(
  "pby_y[0] * iter1",
  sprintf("i_y%d", 2:16)
)

all_x_decl <- sprintf("i_x%d", 2:16)
all_y_decl <- sprintf("i_y%d", 2:16) 

temp <- "

#define MACRO_DIM_<dtype>(DOCODE) do {      \\
  R_xlen_t flatind_out = 0;         \\
  const int *pby_x = INTEGER_RO(by_x);        \\
  const int *pby_y = INTEGER_RO(by_y);        \\
  const double *pdcp_x = REAL_RO(dcp_x);        \\
  const double *pdcp_y = REAL_RO(dcp_y);        \\
  <all_N_decl>
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
        flatind_out++;                      \\
  <endfor>
} while(0)

"

dMacro_skeletons <- character(length(DTYPES))
names(dMacro_skeletons) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_N_decl <- stri_c(all_N_decl[1:i], collapse = "\n")
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_y_decl <- stri_c(all_y_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_N_decl>",
    "<all_x_decl>",
    "<all_y_decl>",
    "<startfor>",
    "<main_x>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_N_decl,
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
# do call ====
#



# cases:
case <-
"case %d:                                       \\
  MACRO_DIM_%d(DOCODE);    \\
  break;                                        \\
"
cases <- sprintf(case, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")


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
# Macro Bind ====
#

# Remember my MISTAKE: cannot use the same iterations for `x` and `out`!!!

BINDTYPES <- seq(2, 16, 2)

all_for <- sprintf(
  "\t for(int iter%d = pstart[%d]; iter%d <= pend[%d]; ++iter%d) {\t\\",
  1:16, 0:15,  1:16, 0:15, 1:16
)
forout <- c(
  "\t\\",
  sprintf("i_out%d = iter%d * pdcp_out[%d];\t\\", 2:16, 2:16, 1:15)
)
forx <- c(
  "\t\\",
  sprintf("i_x%d = pby_x[%d] * (iter%d - pstart[%d]) * pdcp_x[%d];\t\\", 2:16, 1:15, 2:16, 1:15, 1:15)
)
all_for <- stri_c(all_for, forout, forx, sep = "\n")
cat(all_for[16])

all_parts_out <- c(
  "iter1",
  sprintf("i_out%d", 2:16)
)
all_parts_x <- c(
  "pby_x[0] * (iter1 - pstart[0])",
  sprintf("i_x%d", 2:16)
)

all_out_decl <- sprintf("i_out%d", 2:16)
all_x_decl <- sprintf("i_x%d", 2:16) 

temp <- "
#define MACRO_DIM_BIND_<dtype>(DOCODE) do {  \\
  double *pdcp_out = REAL(dcp_out);  \\
  double *pdcp_x = REAL(dcp_x);  \\
                                        \\
  const int *pby_x = INTEGER_RO(by_x);  \\
  const int *pstart = INTEGER_RO(starts); \\
  const int *pend = INTEGER_RO(ends);    \\
  R_xlen_t flatind_out;                 \\
  R_xlen_t flatind_x;                   \\
  R_xlen_t <all_out_decl>;              \\
  R_xlen_t <all_x_decl>;                \\
  <startfor>
        flatind_out = <main_out>;       \\
        flatind_x = <main_x>;           \\
        DOCODE;                         \\
  <endfor>
} while(0)



"

dMacro_skeletons <- character(length(BINDTYPES))
names(dMacro_skeletons) <- BINDTYPES
counter <- 1
for(i in BINDTYPES) {
  
  current_out_decl <- stri_c(all_out_decl[1:(i-1)], collapse = ", ")
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_out <- stri_c(all_parts_out[1:i], collapse = " + ")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_out_decl>",
    "<all_x_decl>",
    "<startfor>",
    "<main_out>",
    "<main_x>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_out_decl,
    current_x_decl,
    current_for,
    current_main_out,
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

cat(dMacro_skeletons[[1]])


macro_dim_bind <- stri_c(dMacro_skeletons, collapse = "\n")


################################################################################
# do call Bind ====
#


# cases:
case <-
  "case %d:                                       \\
  MACRO_DIM_BIND_%d(DOCODE);    \\
  break;                                        \\
"
cases <- sprintf(case, BINDTYPES, BINDTYPES) |> stringi::stri_c(collapse = "")


cat(cases)

templatecode_docall <- "

#define MACRO_DIM_BIND_DOCALL(DOCODE) do {     \\
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


macro_dim_bind_docall <- templatecode_docall2



################################################################################
# Save macros ====
#

macro_dim <- stri_c(
  "\n",
  macro_dim_vector,
  "\n",
  macro_dim_orthovector,
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
  "\n",
  macro_dim_bind,
  "\n",
  macro_dim_bind_docall,
  "\n"
)

readr::write_file(macro_dim, "macro_dim.txt")
