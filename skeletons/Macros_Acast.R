library(stringi)

# MACRO ====

all_N_decl <- sprintf("const int N%d = INTEGER(lens)[%d];\t\\", 1:16, 0:15)
all_ind_decl <- sprintf("const SEXP ind%d = VECTOR_ELT(subs, %d);\t\\", 1:16, 0:15)
all_ind_pointer <- sprintf("const int *pind%d = INTEGER_RO(ind%d);\t\\", 1:16, 1:16)

all_for <- c(
  sprintf("\t for(int iter%d = 0; iter%d < N%d; ++iter%d) {\t\\", 1:16, 1:16, 1:16, 1:16)
)
forout <- sprintf("i_out%d = (pstarts[%d] + iter%d) * pdcp_out[%d];\t\\", 1:16, 0:15, 1:16, 0:15)
fory <- sprintf("i_y%d = (pind%d[iter%d] - 1) * pdcp_y[%d];\t\\", 1:16, 1:16, 1:16, 0:15)
all_for <- stri_c(all_for, forout, fory, sep = "\n")
cat(all_for[1])
cat(all_for[16])


all_parts_out <-  sprintf("i_out%d", 1:16)
all_parts_y <-  sprintf("i_y%d", 1:16)


all_out_decl <- sprintf("i_out%d", 1:16)
all_y_decl <- sprintf("i_y%d", 1:16) 

temp <- "

#define MACRO_DIM_ACAST(DOCODE) do {              \\
  if(Rf_length(subs) < 16) {                     \\
    stop(\"subs not of length 16\");              \\
  }                                               \\
  if(Rf_length(starts) < 16) {                   \\
    stop(\"starts not of length 16\");            \\
  }                                               \\
  if(Rf_length(lens) < 16) {                     \\
    stop(\"lens not of length 16\");              \\
  }                                               \\
  if(Rf_length(dcp_y) < 16) {                    \\
    stop(\"dcp_y not of length 16\");             \\
  }                                               \\
  if(Rf_length(dcp_out) < 16) {                  \\
    stop(\"dcp_out not of length 16\");           \\
  }                                               \\
                                                  \\
  const double *pdcp_out = REAL_RO(dcp_out);      \\
  const double *pdcp_y = REAL_RO(dcp_y);          \\
                                                  \\
  const int *pstarts = INTEGER_RO(starts);        \\
                                                  \\
                                                  \\
  <all_N_decl>
                                                  \\
  <all_ind_decl>
                                                  \\
  <all_ind_pointer>
                                                  \\
  R_xlen_t flatind_out;       \\
  R_xlen_t flatind_y;       \\
  R_xlen_t <all_out_decl>; \\
  R_xlen_t <all_y_decl>; \\
  <startfor>
        flatind_out = <main_out>;       \\
        flatind_y = <main_y>;     \\
                                                                    \\
        DOCODE;                                                          \\
  	                                                                \\
        flatind_out++;                      \\
  <endfor>
} while(0)

"

i <- 16L

current_N_decl <- stri_c(all_N_decl[1:i], collapse = "\n")
current_ind_decl <- stri_c(all_ind_decl[1:i], collapse = "\n")
current_ind_pointer <- stri_c(all_ind_pointer[1:i], collapse = "\n")
current_out_decl <- stri_c(all_out_decl[1:(i)], collapse = ", ")
current_y_decl <- stri_c(all_y_decl[1:(i)], collapse = ", ")
current_for <- stri_c(all_for[i:1], collapse = "\n")
current_main_out <- stri_c(all_parts_out[1:i], collapse = " + ")
current_main_y <- stri_c(all_parts_y[1:i], collapse = " + ")
current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")

current_fixed <- c(
  "<dtype>",
  "<all_N_decl>",
  "<all_ind_decl>",
  "<all_ind_pointer>",
  "<all_out_decl>",
  "<all_y_decl>",
  "<startfor>",
  "<main_out>",
  "<main_y>",
  "<endfor>"
)
current_replacement <- c(
  i,
  current_N_decl,
  current_ind_decl,
  current_ind_pointer,
  current_out_decl,
  current_y_decl,
  current_for,
  current_main_out,
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

macro_dim_acast <- out
cat(macro_dim_acast)

macro_op_acast_loop <- "

#define MACRO_OP_ACAST_LOOP(DOCODE) do {                  \\
                                                            \\
  for(int i = 0; i < grp_n; ++i) {                          \\
    grp_count = rcpp_factor_count(grp, i + 1);              \\
    if(grp_count > 0) {                                       \\
      grp_which = rcpp_factor_which(grp, i + 1, grp_count);   \\
      plens[margin] = grp_count;                              \\
      SET_VECTOR_ELT(subs, margin, grp_which);                \\
      DOCODE;                                                 \\
      pstarts[newdim] = pstarts[newdim] + 1;                  \\
    }                                                         \\
  }                                                         \\
} while(0)

"

macro_op_acast <- "
#define MACRO_OP_ACAST do {       \\
  switch(TYPEOF(out)) {	\\
    case LGLSXP:	\\
    {	\\
      int *py = LOGICAL(y);                                 \\
      int *pout = LOGICAL(out);                             \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \\
      break;                                                \\
    }	\\
    case INTSXP:	\\
    {	\\
      int *py = INTEGER(y);                                 \\
      int *pout = INTEGER(out);                             \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \\
      break;                                                \\
    }	\\
    case REALSXP:	\\
    {	\\
      double *py = REAL(y);                                     \\
      double *pout = REAL(out);                                 \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));       \\
      break;                                                \\
    }	\\
    case CPLXSXP:	\\
    {	\\
      Rcomplex *py = COMPLEX(y);                                 \\
      Rcomplex *pout = COMPLEX(out);                             \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \\
      break;                                                \\
    }	\\
    case STRSXP:	\\
    {	\\
      const SEXP *py = STRING_PTR_RO(y);                                 \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(SET_STRING_ELT(out, flatind_out, py[flatind_y])));   \\
      break;                                                \\
    }	\\
    case RAWSXP:	\\
    {	\\
      Rbyte *py = RAW(y);	                                  \\
      Rbyte *pout = RAW(out);	                              \\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(pout[flatind_out] = py[flatind_y]));   \\
      break;                                                \\
    }	\\
    case VECSXP:	\\
    {	\\
      MACRO_OP_ACAST_LOOP(MACRO_DIM_ACAST(                                                \\
        SET_VECTOR_ELT(out, flatind_out, VECTOR_ELT(y, flatind_y))  \\
      ));                                                              \\
      break;                                                \\
    }	\\
    default:	\\
    {	\\
      stop(\"unsupported type\");	\\
    }	\\
  }	\\
} while(0)
"

macro_acast <- stri_c(
  macro_dim_acast,
  macro_op_acast_loop,
  macro_op_acast,
  collapse = "\n\n"
)

readr::write_file(macro_acast, "macro_acast.txt")
