# set-up ====

library(stringi)

DTYPES <- c(4L, 16L)
BINDTYPES <- c(16L)


################################################################################
# Introduction ====
#

introcomments <- "


********************************************************************************
MACROs for broadcasted element-wise binary operations

The following MACROs define the loops used for broadcasted element-wise binary operations.

In the context of a broadcasted operation involving exactly 2 arrays,
'broadcast' uses different techniques for looping through the elements for broadcasting.
The techniques are the following, ordered from high to low priority:
 1) broadcasting where one of the arrays is a vector
 2) regular broadcasting

The dimensions of both arrays are first NORMALIZED and SIMPLIFIED (see 'R' code),
before determining which technique to use.

'vector broadcasting' occurs when at least one of the following is true:
 - x and/or y is a scalar (i.e. length of 1)
 - x and y are vectors or 1d array (i.e. ndims() <= 1L)
 - x and y have the exact same dimensions

When vector broadcasting does not hold,
'ortho-vector broadcasting' occurs when the following is true:
 - x is a row-vector and y is a column-vector, or vice-versa

When both vector and orth-vector broadcasting does not hold,
'big-to-vector' broadcasting occurs when ALL of the following is true
(again, AFTER normalization and simplification):
 - the arrays have 2 or 3 dimensions
 - x is a vector or y is a vector (i.e. only one dimension has size > 1)
 - all(dim(x) > dim(y)) || all(dim(y) > dim(x))
 - if the larger array is a 3d array, the smaller array had dimension in the form c(1, n, 1)

When none of the above techniques hold, The regular broadcasting technique is used.
The MACROs for regular broadcasting were written for 4 and 16 dimensions.
These MACROs were written via a simple 'R' script,
to minimize the risk of human error.

For broadcasting dimmodes 'big-to-vector' and 'regular'
the dimensions of the involved arrays are internally chunkified,
to ensure they fit the MACROs.
This has some overhead, but not too much.


********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)


################################################################################
# Vector Modes ====
#

macro_dim_vectorspecial <- "

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
  }                                                                 \\
  for(R_xlen_t flatind_out = 0; flatind_out < nout; ++flatind_out) {  \\
    DOCODE;                                                           \\
    flatind_x = flatind_x + by_x;                                     \\
    flatind_y = flatind_y + by_y;                                     \\
  }                                                                   \\
                                                                      \\
} while(0)



#define MACRO_DIM_ORTHOVECTOR(DOCODE) do {      \\
  R_xlen_t flatind_out = 0;         \\
  const int N1 = INTEGER_RO(out_dim)[0];      \\
  const int N2 = INTEGER_RO(out_dim)[1];       \\
  bool RxC = INTEGER_RO(x_dim)[0] != 1; \\
  if(RxC) { \\
    for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {	\\
  	  for(int flatind_x = 0; flatind_x < N1; ++flatind_x) {	\\
        DOCODE;                         \\
        ++flatind_out;                      \\
    	 }	\\
  	 }	\\
  } \\
  else {  \\
    for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {	\\
    	  for(int flatind_y = 0; flatind_y < N1; ++flatind_y) {	\\
          DOCODE;                         \\
          ++flatind_out;                      \\
        }	\\
    }	\\
  } \\
} while(0)



#define MACRO_DIM_BIG2VECTOR(DOCODE) do {      \\
  const int N1 = INTEGER_RO(out_dim)[0];    \\
  const int N2 = INTEGER_RO(out_dim)[1];    \\
  const int N3 = INTEGER_RO(out_dim)[2];    \\
  if(vectorx) { \\
    R_xlen_t flatind_y = 0;                                   \\
    R_xlen_t flatind_out = 0;                                 \\
    for(int iter3 = 0; iter3 < N3; ++iter3) {                 \\
      for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {   \\
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \\
          DOCODE;                                             \\
          ++flatind_y;                                        \\
          ++flatind_out;                                      \\
        }                                                     \\
      }                                                       \\
    }                                                         \\
  } \\
  else {  \\
    R_xlen_t flatind_x = 0;                                   \\
    R_xlen_t flatind_out = 0;                                 \\
    for(int iter3 = 0; iter3 < N3; ++iter3) {                 \\
      for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {   \\
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \\
          DOCODE;                                             \\
          ++flatind_x;                                        \\
          ++flatind_out;                                      \\
        }                                                     \\
      }                                                       \\
    }                                                         \\
  } \\
} while(0)


#define MACRO_DIM_SANDWICH2VECTOR(DOCODE) do {      \\
  \\
  if(vectorx) {  \\
    R_xlen_t flatind_out = 0; \\
    const int *pydim = INTEGER_RO(y_dim); \\
    const R_xlen_t stride_y = (double)pydim[0] * (double)pydim[1];  \\
    const R_xlen_t N1 = INTEGER_RO(out_dim)[0];  \\
    const int N2 = INTEGER_RO(out_dim)[1];  \\
    const R_xlen_t N3 = (R_xlen_t)INTEGER_RO(out_dim)[2] * stride_y;  \\
    R_xlen_t flatind_y;\\
    for(R_xlen_t iter3 = 0; iter3 < N3; iter3 += stride_y) {  \\
      for(int flatind_x = 0; flatind_x < N2; ++flatind_x) { \\
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \\
          flatind_y = iter3 + iter1; \\
            DOCODE; \\
            ++flatind_out;  \\
        } \\
      } \\
    } \\
  } \\
  else {  \\
    R_xlen_t flatind_out = 0; \\
    const int *pxdim = INTEGER_RO(x_dim); \\
    const R_xlen_t stride_x = (double)pxdim[0] * (double)pxdim[1];  \\
    const R_xlen_t N1 = INTEGER_RO(out_dim)[0];  \\
    const int N2 = INTEGER_RO(out_dim)[1];  \\
    const R_xlen_t N3 = INTEGER_RO(out_dim)[2] * stride_x;  \\
    R_xlen_t flatind_x;\\
    for(R_xlen_t iter3 = 0; iter3 < N3; iter3 += stride_x) {  \\
      for(int flatind_y = 0; flatind_y < N2; ++flatind_y) { \\
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \\
          flatind_x = iter3 + iter1; \\
            DOCODE; \\
            ++flatind_out;  \\
        } \\
      } \\
    } \\
  } \\
} while(0)



#define MACRO_DIM_VECTORSPECIAL(DOCODE) do {  \\
  if(dimmode == 1) {  \\
    MACRO_DIM_VECTOR(DOCODE); \\
  } \\
  else if(dimmode == 2) { \\
    MACRO_DIM_ORTHOVECTOR(DOCODE);  \\
  } \\
  else if(dimmode == 3) { \\
    MACRO_DIM_BIG2VECTOR(DOCODE); \\
  } \\
  else if(dimmode == 4) { \\
    MACRO_DIM_SANDWICH2VECTOR(DOCODE); \\
  } \\
  else {  \\
    stop(\"dimmode is not a vector mode\"); \\
  } \\
} while(0)


"


################################################################################
# General ====
#


all_N_decl <- sprintf("const int N%d = INTEGER_RO(out_dim)[%d];\t\\", 1:16, 0:15)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d <N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
)
forx <- c(
  "\t\\",
  sprintf("i_x%d = pby_x[%d] * iter%d * pdcp_x[%d] + i_x%d;\t\\",
          2:16, 1:15, 2:16, 1:15, 3:17)
)
fory <- c(
  "\t\\",
  sprintf("i_y%d = pby_y[%d] * iter%d * pdcp_y[%d] + i_y%d;\t\\",
          2:16, 1:15, 2:16, 1:15, 3:17)
)
all_for <- stri_c(all_for, forx, fory, sep = "\n")
cat(all_for[16])

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
  \\
  R_xlen_t flatind_x;       \\
  R_xlen_t flatind_y;       \\
  R_xlen_t <all_x_decl>; \\
  R_xlen_t <all_y_decl>; \\
  <startfor>
        flatind_x = pby_x[0] * iter1 + i_x2;       \\
        flatind_y = pby_y[0] * iter1 + i_y2;     \\
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
  find <- sprintf(c(" + i_x%d", " + i_y%d"), i + 1)
  current_for <- stri_replace_all(
    current_for, c("", ""), fixed = find, vectorise_all = FALSE
  )
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_N_decl>",
    "<all_x_decl>",
    "<all_y_decl>",
    "<startfor>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_N_decl,
    current_x_decl,
    current_y_decl,
    current_for,
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
# do call ====
#



# cases:
case <-
  "case %d:                                       \\
  MACRO_DIM_DEC_%d(DOCODE);    \\
  break;                                        \\
"
cases <- sprintf(case, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")


cat(cases)

templatecode_docall <- "

#define MACRO_DIM_DEC_DOCALL(DOCODE) do {     \\
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


macro_dim_dec_docall <- templatecode_docall2




################################################################################
# Intro 2 ====
#

introcomments2 <- "

********************************************************************************
MACROs for the binding implementation

The following MACROs define the loops used for broadcasted binding.

The MACROs were written for 16 dimensions.
These MACROs were written via a simple 'R' script,
to minimize the risk of human error.

********************************************************************************

"

introcomments2 <- stri_split(introcomments2, fixed = "\n")[[1]]
introcomments2 <- stri_c("// ", introcomments2) |> paste0(collapse = "\n")
cat(introcomments2)



################################################################################
# Macro Bind ====
#



all_start_decl <- sprintf("const int start%d = INTEGER_RO(starts)[%d];\t\\", 1:16, 0:15)
all_end_decl <- sprintf("const int end%d = INTEGER_RO(ends)[%d];\t\\", 1:16, 0:15)


all_for <- sprintf(
  "\t for(int iter%d = start%d; iter%d <= end%d; ++iter%d) {\t\\",
  1:16, 1:16,  1:16, 1:16, 1:16
)
forout <- c(
  "\t\\",
  sprintf("i_out%d = iter%d * pdcp_out[%d] + i_out%d;\t\\",
          2:16, 2:16, 1:15, 3:17)
)
forx <- c(
  "\t\\",
  sprintf("i_x%d = pby_x[%d] * (iter%d - start%d) * pdcp_x[%d] + i_x%d;\t\\",
          2:16, 1:15, 2:16, 2:16, 1:15, 3:17)
)
all_for <- stri_c(all_for, forout, forx, sep = "\n")
cat(all_for[16])


all_out_decl <- sprintf("i_out%d", 2:16)
all_x_decl <- sprintf("i_x%d", 2:16) 

temp <- "
#define MACRO_DIM_BIND_<dtype>(DOCODE) do {  \\
  const int *pby_x = INTEGER_RO(by_x);  \\
  const double *pdcp_out = REAL_RO(dcp_out);  \\
  const double *pdcp_x = REAL_RO(dcp_x);  \\
                                  \\
  <all_start_decl>
  <all_end_decl>
                                        \\
  R_xlen_t flatind_out;                 \\
  R_xlen_t flatind_x;                   \\
  R_xlen_t <all_out_decl>;              \\
  R_xlen_t <all_x_decl>;                \\
  <startfor>
        flatind_out = iter1 + i_out2;       \\
        flatind_x = pby_x[0] * (iter1 - start1) + i_x2;           \\
        DOCODE;                         \\
  <endfor>
} while(0)



"

dMacro_skeletons <- character(length(BINDTYPES))
names(dMacro_skeletons) <- BINDTYPES
counter <- 1
for(i in BINDTYPES) {
  
  current_start_decl <- stri_c(all_start_decl[1:i], collapse = "\n")
  current_end_decl <- stri_c(all_end_decl[1:i], collapse = "\n")
  
  current_out_decl <- stri_c(all_out_decl[1:(i-1)], collapse = ", ")
  current_x_decl <- stri_c(all_x_decl[1:(i-1)], collapse = ", ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  find <- sprintf(c(" + i_x%d", " + i_out%d"), i + 1)
  current_for <- stri_replace_all(
    current_for, c("", ""), fixed = find, vectorise_all = FALSE
  )
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<all_start_decl>",
    "<all_end_decl>",
    "<all_out_decl>",
    "<all_x_decl>",
    "<startfor>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_start_decl,
    current_end_decl,
    current_out_decl,
    current_x_decl,
    current_for,
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

# 
# 
# ################################################################################
# # Intro 3 ====
# #
# 
# introcomments3 <- "
# 
# ********************************************************************************
# MACROs for the set implementation
# 
# The following MACROs define the loops used for broadcasted in-place modification.
# 
# The MACROs were written for every 2 dimensions, from 2 to 16.
# i.e. 2, 4, 6, ..., 16
# 
# ********************************************************************************
# 
# "
# 
# introcomments3 <- stri_split(introcomments3, fixed = "\n")[[1]]
# introcomments3 <- stri_c("// ", introcomments3) |> paste0(collapse = "\n")
# cat(introcomments3)
# 
# 
# 
# ################################################################################
# # MACRO SET ====
# #
# 
# 
# all_N_decl <- sprintf("const int N%d = INTEGER(x_dim)[%d];\t\\", 1:16, 0:15)
# 
# all_for <- c(
#   sprintf("\t for(int iter%d = 0; iter%d < N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
# )
# fory <- c(
#   "\t\\",
#   sprintf("i_y%d = pby_y[%d] * iter%d * pdcp_y[%d];\t\\", 2:16, 1:15, 2:16, 1:15)
# )
# all_for <- stri_c(all_for, fory, sep = "\n")
# cat(all_for[16])
# 
# all_parts_y <- c(
#   "iter1 * pby_y[0]",
#   sprintf("i_y%d", 2:16)
# )
# 
# all_y_decl <- sprintf("i_y%d", 2:16) 
# 
# temp <- "
# 
# #define MACRO_DIM_SET_<dtype>(DOCODE) do {      \\
#   R_xlen_t flatind_x = 0;         \\
#   const int *pby_y = INTEGER_RO(by_y);        \\
#   <all_N_decl>
#   const double *pdcp_y = REAL_RO(dcp_y);        \\
#   R_xlen_t flatind_y;       \\
#   R_xlen_t <all_y_decl>; \\
#   <startfor>
#         flatind_y = <main_y>;     \\
#                                   \\
#         DOCODE;                   \\
#   	                              \\
#         flatind_x++;                    \\
#   <endfor>
# } while(0)
# 
# "
# 
# dMacro_skeletons <- character(length(DTYPES))
# names(dMacro_skeletons) <- DTYPES
# counter <- 1
# for(i in DTYPES) {
#   
#   current_N_decl <- stri_c(all_N_decl[1:i], collapse = "\n")
#   current_y_decl <- stri_c(all_y_decl[1:(i-1)], collapse = ", ")
#   current_for <- stri_c(all_for[i:1], collapse = "\n")
#   current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
#   current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
#   
#   current_fixed <- c(
#     "<dtype>",
#     "<all_N_decl>",
#     "<all_y_decl>",
#     "<startfor>",
#     "<main_y>",
#     "<endfor>"
#   )
#   current_replacement <- c(
#     i,
#     current_N_decl,
#     current_y_decl,
#     current_for,
#     current_main_y,
#     current_end
#   )
#   
#   out <- stri_replace_all(
#     temp,
#     fixed = current_fixed,
#     replacement = current_replacement,
#     case_insensitive = FALSE,
#     vectorize_all = FALSE
#   )
#   
#   dMacro_skeletons[counter] <- out
#   counter <- counter + 1
# }
# 
# cat(dMacro_skeletons[[2]])
# 
# 
# macro_dim_set <- stri_c(dMacro_skeletons, collapse = "\n")
# 
# 
# 
# ################################################################################
# # DoCall set skeleton ====
# #
# 
# 
# 
# # cases:
# case_set <-
#   "case %d:                                       \\
#   MACRO_DIM_SET_%d(DOCODE);    \\
#   break;                                        \\
# "
# cases_set <- sprintf(case_set, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")
# 
# 
# cat(cases_set)
# 
# templatecode_docall <- "
# 
# #define MACRO_DIM_SET_DOCALL(DOCODE) do {     \\
#   int ndims = Rf_length(x_dim);         \\
#                                           \\
#   switch(ndims) {                       \\
#     <cases_set>                     \\
#   }                                     \\
# } while(0)"
# 
# templatecode_docall2 <- stringi::stri_replace_all(
#   templatecode_docall,
#   fixed = c("<cases_set>"),
#   replacement = c(cases_set),
#   vectorize_all = FALSE
# )
# 
# 
# cat(templatecode_docall2)
# 
# 
# macro_dim_set_docall <- templatecode_docall2
# 

################################################################################
# Save macros ====
#

macro_dim <- stri_c(
  "\n",
  introcomments,
  "\n",
  macro_dim_vectorspecial,
  "\n",
  macro_dim_d,
  "\n",
  macro_dim_docall,
  "\n",
  introcomments2,
  "\n",
  macro_dim_bind,
  "\n",
  macro_dim_bind_docall,
  "\n"
  # introcomments3,
  # "\n",
  # macro_dim_set,
  # "\n",
  # macro_dim_set_docall,
  # "\n"
)

readr::write_file(macro_dim, "macro_dim.txt")
