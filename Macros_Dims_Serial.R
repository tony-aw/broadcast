# set-up ====

library(stringi)



################################################################################
# DimMacro vector ====
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
# Macro Orthogonal XSTARTS ====
#

DTYPES <- c(2:16)

all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 16:1, 16:1, 15:0, 16:1)
)
all_parts_x <- c(
  "iter1",
  sprintf("iter%d * pdcp_x[%d]", 2:16, 1:15)
)
all_parts_y <- c(
  "iter1",
  sprintf("iter%d * pdcp_y[%d]", 2:16, 1:15)
)

temp <- "

#define MACRO_DIM_ORTHO_XSTARTS_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y;       \\
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
for(i in DTYPES) {
  
  xseq <- seq(1, i, 2)
  yseq <- seq(2, i, 2)
  
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[xseq], collapse = " + ")
  current_main_y <- stri_c(all_parts_y[yseq], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<startfor>",
    "<main_x>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
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
  
  dMacro_skeletons[i-1] <- out
}

cat(dMacro_skeletons[[15]])

macro_dim_ortho_xstarts <- stri_c(dMacro_skeletons, collapse = "\n")




################################################################################
# Macro Orthogonal YSTARTS ====
#

DTYPES <- c(2:16)

all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 16:1, 16:1, 15:0, 16:1)
)


all_parts_x <- c(
  "iter1",
  sprintf("iter%d * pdcp_x[%d]", 2:16, 1:15)
)
all_parts_y <- c(
  "iter1",
  sprintf("iter%d * pdcp_y[%d]", 2:16, 1:15)
)


temp <- "

#define MACRO_DIM_ORTHO_YSTARTS_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y;       \\
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
for(i in DTYPES) {
  
  yseq <- seq(1, i, 2)
  xseq <- seq(2, i, 2)
  
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[xseq], collapse = " + ")
  current_main_y <- stri_c(all_parts_y[yseq], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<startfor>",
    "<main_x>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
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
  
  dMacro_skeletons[i-1] <- out
}

cat(dMacro_skeletons[[3]])

macro_dim_ortho_ystarts <- stri_c(dMacro_skeletons, collapse = "\n")



################################################################################
# Macro DoCall Ortho skeleton ====
#


# cases:
case_xstarts <-
  "case %d:                                       \\
  MACRO_DIM_ORTHO_XSTARTS_%d(DOCODE);    \\
  break;                                        \\
"
cases_xstarts <- sprintf(case_xstarts, 2:16, 2:16) |> stringi::stri_c(collapse = "")

case_ystarts <-
  "case %d:                                       \\
  MACRO_DIM_ORTHO_YSTARTS_%d(DOCODE);    \\
  break;                                        \\
"
cases_ystarts <- sprintf(case_ystarts, 2:16, 2:16) |> stringi::stri_c(collapse = "")


cat(cases_xstarts)
cat(cases_ystarts)

templatecode_docall <- "

#define MACRO_DIM_ORTHO_DOCALL(DOCODE) do {     \\
  int ndims = Rf_length(out_dim);         \\
                                          \\
  if(xstarts) {                           \\
    switch(ndims) {                       \\
      <cases_xstarts>                     \\
    }                                     \\
  }                                       \\
  else {                                  \\
    switch(ndims) {                       \\
      <cases_ystarts>                     \\
    }                                     \\
  }                                       \\
} while(0)"

templatecode_docall2 <- stringi::stri_replace_all(
  templatecode_docall,
  fixed = c("<cases_xstarts>", "<cases_ystarts>"),
  replacement = c(cases_xstarts, cases_ystarts),
  vectorize_all = FALSE
)


cat(templatecode_docall2)


macro_dim_ortho_docall <- templatecode_docall2



################################################################################
# d2-8 Macro skeleton ====
#

DTYPES <- c(2:8, 16)

all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < pout_dim[%d]; ++iter%d) {\t\\", 16:1, 16:1, 15:0, 16:1)
)
all_parts_x <- c(
  "iter1 * pby_x[0]",
  sprintf("pdcp_x[%d] * (iter%d * pby_x[%d])", 1:15, 2:16, 1:15)
)
all_parts_y <- c(
  "iter1 * pby_y[0]",
  sprintf("pdcp_y[%d] * (iter%d * pby_y[%d])", 1:15, 2:16, 1:15)
)

temp <- "

#define MACRO_DIM_<dtype>(DOCODE) do {      \\
  R_xlen_t counter = 0;         \\
  const int *pby_x = INTEGER_RO(by_x);        \\
  const int *pby_y = INTEGER_RO(by_y);        \\
  const int *pout_dim = INTEGER_RO(out_dim);      \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y;       \\
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
  
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main_x <- stri_c(all_parts_x[1:i], collapse = " + ")
  current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<startfor>",
    "<main_x>",
    "<main_y>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
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


macro_dim_d <- stri_c(dMacro_skeletons[1:7], collapse = "\n")



################################################################################
# do call Macro skeleton ====
#


# cases:
case <-
"case %d:                                       \\
  MACRO_DIM_%d(DOCODE);    \\
  break;                                        \\
"
cases <- sprintf(case, 2:8, 2:8) |> stringi::stri_c(collapse = "")


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
# DimMacro general ====
#

# subs = indices (maybe no longer needed in the future?)
# bounds = index lengths = out.dim
# indx = linear index for subs; i.e. subs[indx]

macro_dim_general <- "


#define MACRO_DIM_GENERAL(DOCODE) do {                              \\
  const void *vmaxsave = vmaxget();                                 \\
                                                                    \\
  int k = Rf_length(xdims1);                                        \\
  const int *pxdims1 = INTEGER_RO(xdims1);                          \\
  const int *pxdims2 = INTEGER_RO(xdims2);                          \\
                                                                    \\
  int **subs1 = (int**)R_alloc(k, sizeof(int*));                    \\
  int *indx1 = (int*)R_alloc(k, sizeof(int));                       \\
  int *bound1 = (int*)R_alloc(k, sizeof(int));                      \\
  R_xlen_t *offset1 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));      \\
                                                                    \\
  int **subs2 = (int**)R_alloc(k, sizeof(int*));                    \\
  int *indx2 = (int*)R_alloc(k, sizeof(int));                       \\
  int *bound2 = (int*)R_alloc(k, sizeof(int));                      \\
  R_xlen_t *offset2 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));      \\
                                                                    \\
                                                                    \\
  R_xlen_t n1 = 1;                                                  \\
  SEXP r1;                                                          \\
  R_xlen_t n2 = 1;                                                  \\
  SEXP r2;                                                          \\
                                                                    \\
  for (int i = 0; i < k; i++) {                                     \\
    r1 = VECTOR_ELT(s1, i);                                         \\
  	indx1[i] = 0;                                                   \\
  	bound1[i] = Rf_length(r1);                                      \\
    n1 *= bound1[i];                                                \\
  	subs1[i] = INTEGER(r1);                                         \\
  	                                                                \\
  	r2 = VECTOR_ELT(s2, i);                                         \\
  	indx2[i] = 0;                                                   \\
  	bound2[i] = Rf_length(r2);                                      \\
    n2 *= bound2[i];                                                \\
  	subs2[i] = INTEGER(r2);                                         \\
  }                                                                 \\
                                                                    \\
  offset1[0] = 1;                                                   \\
  offset2[0] = 1;                                                   \\
  for (int i = 1; i < k; i++) {                                     \\
    offset1[i] = offset1[i - 1] * pxdims1[i - 1];                   \\
    offset2[i] = offset2[i - 1] * pxdims2[i - 1];                   \\
  }                                                                 \\
                                                                    \\
                                                                    \\
  R_xlen_t counter = 0;                                             \\
  R_xlen_t flatind_x;                                               \\
  R_xlen_t flatind_y;                                               \\
                                                                    \\
  for (R_xlen_t i = 0; i < nout; i++) {                             \\
                                                                    \\
    flatind_x = 0;                                                  \\
    flatind_y = 0;                                                  \\
                                                                    \\
  	for (int j = 0; j < k; j++) {                                   \\
	    int jj1 = subs1[j][indx1[j]];                                 \\
	    flatind_x += (jj1 - 1) * offset1[j];                          \\
	    int jj2 = subs2[j][indx2[j]];                                 \\
	    flatind_y += (jj2 - 1) * offset2[j];                          \\
  	}                                                               \\
  	                                                                \\
  	                                                                \\
  	DOCODE;                                                         \\
  	                                                                \\
  	pout[counter] = tempout;                                        \\
    counter++;                                                      \\
  	                                                                \\
	  int j1 = 0;                                                     \\
    while (++indx1[j1] >= bound1[j1]) {                             \\
  		indx1[j1] = 0;                                                \\
  		j1 = (j1 + 1) % k;                                            \\
    }                                                               \\
  	int j2 = 0;                                                     \\
    while (++indx2[j2] >= bound2[j2]) {                             \\
  		indx2[j2] = 0;                                                \\
  		j2 = (j2 + 1) % k;                                            \\
    }                                                               \\
	}                                                                 \\
                                                                    \\
                                                                    \\
  vmaxset(vmaxsave);                                                \\
                                                                    \\
} while(0)

"




################################################################################
# Save macros ====
#

macro_dim <- stri_c(
  macro_dim_vector,
  "\n",
  macro_dim_ortho_xstarts,
  "\n",
  macro_dim_ortho_ystarts,
  "\n",
  macro_dim_ortho_docall,
  "\n",
  macro_dim_d,
  "\n",
  macro_dim_docall,
  "\n",
  macro_dim_general
)

readr::write_file(macro_dim, "macro_dim.txt")
