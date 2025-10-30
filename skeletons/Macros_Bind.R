# set-up ====

library(stringi)

DTYPES <- c(4L, 16L)
BINDTYPES <- c(4L, 16L)


################################################################################
# Macro Dims ====
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

cat(dMacro_skeletons[[2]])


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
  introcomments,
  "\n",
  macro_dim_vector,
  "\n",
  macro_dim_orthovector,
  "\n",
  macro_dim_big2vector,
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
