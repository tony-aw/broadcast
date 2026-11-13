# set-up ====

library(stringi)

DTYPES <- 16L
BINDTYPES <- c(16L)


################################################################################
# Introduction ====
#

introcomments <- "


********************************************************************************
MACROs for broadcasted binary operations where one or both sides are vectors

The following MACROs define the loops used for broadcasted element-wise binary operations,
specifically where one or both sides are (oriented) vectors.

The dimensions of both arrays are first NORMALIZED and SIMPLIFIED virtually
(see 'rcpp_virt_binary_prep' code),
before determining which technique to use.


********************************************************************************

"

introcomments <- stri_split(introcomments, fixed = "\n")[[1]]
introcomments <- stri_c("// ", introcomments) |> paste0(collapse = "\n")
cat(introcomments)



################################################################################
# Two-Vectors Mode ====
#


macro_dim_twovectors <- "

#define MACRO_DIM_VECTOR(DOCODE, GETX, GETY) do {                               \\
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
    GETX; \\
    GETY; \\
    DOCODE;                                                           \\
    flatind_x += by_x;                                     \\
    flatind_y += by_y;                                     \\
  }                                                                   \\
                                                                      \\
} while(0)



#define MACRO_DIM_ORTHOVECTOR(DOCODE, GETX, GETY) do {      \\
  R_xlen_t flatind_out = 0;         \\
  const int N1 = INTEGER_RO(out_dim)[0];      \\
  const int N2 = INTEGER_RO(out_dim)[1];       \\
  bool RxC = INTEGER_RO(x_dim)[0] != 1; \\
  if(RxC) { \\
    for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {	\\
      GETY; \\
  	  for(int flatind_x = 0; flatind_x < N1; ++flatind_x) {	\\
  	    GETX; \\
        DOCODE;                         \\
        ++flatind_out;                      \\
    	 }	\\
  	 }	\\
  } \\
  else {  \\
    for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {	\\
      GETX; \\
    	  for(int flatind_y = 0; flatind_y < N1; ++flatind_y) {	\\
    	    GETY; \\
          DOCODE;                         \\
          ++flatind_out;                      \\
        }	\\
    }	\\
  } \\
} while(0)



#define MACRO_DIM_TWOVECTORS(DOCODE, GETX, GETY) do {  \\
  if(dimmode == 1) {  \\
    MACRO_DIM_VECTOR(DOCODE, GETX, GETY); \\
  } \\
  else if(dimmode == 2) { \\
    MACRO_DIM_ORTHOVECTOR(DOCODE, GETX, GETY);  \\
  } \\
  else {  \\
    stop(\"dimmode is not a two-vector mode\"); \\
  } \\
} while(0)



"


################################################################################
# One-Vector Modes ====
#

macro_dim_onevector <- "

#define MACRO_DIM_BIG2VECTOR(DOCODE, GETX, GETY) do {      \\
  const int N1 = INTEGER_RO(out_dim)[0];    \\
  const int N2 = INTEGER_RO(out_dim)[1];    \\
  const int N3 = INTEGER_RO(out_dim)[2];    \\
  \\
  if(vectorx) { \\
    R_xlen_t flatind_y = 0;                                   \\
    R_xlen_t flatind_out = 0;                                 \\
    for(int iter3 = 0; iter3 < N3; ++iter3) {                 \\
      for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {   \\
        GETX;                                                 \\
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \\
          GETY;                                               \\
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
        GETY;                                                 \\
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \\
          GETX;                                               \\
          DOCODE;                                             \\
          ++flatind_x;                                        \\
          ++flatind_out;                                      \\
        }                                                     \\
      }                                                       \\
    }                                                         \\
  } \\
} while(0)


#define MACRO_DIM_SANDWICH2VECTOR(DOCODE, GETX, GETY) do {      \\
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
        GETX; \\
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \\
          flatind_y = iter3 + iter1; \\
          GETY; \\
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
        GETY; \\
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \\
          flatind_x = iter3 + iter1; \\
          GETX; \\
          DOCODE; \\
          ++flatind_out;  \\
        } \\
      } \\
    } \\
  } \\
} while(0)



#define MACRO_DIM_ONEVECTOR(DOCODE, GETX, GETY) do {  \\
  bool vectorx = inline_dims_is_vector(x_dim);  \\
  \\
  if(dimmode == 3) { \\
    MACRO_DIM_BIG2VECTOR(DOCODE, GETX, GETY); \\
  } \\
  else if(dimmode == 4) { \\
    MACRO_DIM_SANDWICH2VECTOR(DOCODE, GETX, GETY); \\
  } \\
  else {  \\
    stop(\"dimmode is not a one-vector mode\"); \\
  } \\
} while(0)


"



################################################################################
# Intro 2 ====
#


introcomments2 <- "

********************************************************************************
MACROs for the odometer

The following MACROs define the loops used for general broadcasted binary operations

The odometer itself works basically as follows:


inline void odometer_step(
  int ndim, int *iters, const int *N_bounds,
  R_xlen_t *current_x, R_xlen_t *current_y,
  const R_xlen_t *stride_x, const R_xlen_t *stride_y,
  const R_xlen_t *contrib_x, const R_xlen_t *contrib_y
) {
  
  // basic relationship between flat index at one hand,
  // and the dimensional iterations and strides at the other hand:
  // flatind = iter1 * stride1 + iter2 * stride2 + etc.
  
  // at start, all iters are 0, thus flatind is 0:
  // flatind = 0 + 0 + etc. == 0.
  
  // as we go through the rows, flatind increases with iter1 * stride1:
  // flatind = iter1 * stride1
  
  // when all rows (d=0) have been passed,
  // we need to add the contribution of going to the next column:
  // (iter2 = 1) * stride2; this is simply +stride2
  // AND we need to RESET the row's contribution,
  // meaning we need to REMOVE what the rows have accumulated:
  // remove `iter1 * stride1 * (N1 - 1)`

  // then we once again accumulate rows.
  // everytime we've gone through all rows:
  // - REMOVE the contribution to the rows; i.e. remove `iter1 * stride1 * (N1 - 1)`
  // - we arleady have iter2=1; now we need iter2 = 2, so add +stride2 to the flatind.
  
  // and so we go on.
  
  // at some point, we've gone through all the columns and rows.
  // then we do to the columns what we do with the rows:
  // - remove the contribution of the columns
  // - remove the contribution of the rows also
  // - we need to add the contribution of going to the next layer:
  //      (iter3 = 1) * stride3; this is simply +stride3

  // and so we continue
  
  // notice that after each cycle, flatind is pushed back by the stride of the current dimension.
  // but flatind never gets pushed back further than the stride of one dimension.
  // this, in a lighting-shape, we eventually keep moving forward, not backward.
  // Like a cartesian product

    // We start at d = 1, since d=0 represents the rows
    
  for (int d = 1; d < ndim; ++d) {
    
    iters[d]++; // this always start with increasing the column iterator;
    // whether we can even reach further dimensions depends on the if-else statements below...
    
    if (iters[d] < N_bounds[d]) {
      // do the following if we have NOT gone through all iters of dimension `d`
    
      *current_x += stride_x[d]; // add contribution from column, layer, etc.
      *current_y += stride_y[d]; // add contribution from column, layer, etc.
      break; // done; do NOT run for the next `d`s
        
    } else {
      // dimension `d` (column, layer, etc.) is done; time to....
      iters[d] = 0; // reset iterator of current dimension (column, layer, etc.)
      *current_x -= contrib_x[d]; // remove its contribution
      *current_y -= contrib_y[d]; // remove its contribution
        
      // NO BREAK! we'll continue to d+1...
    }
  }
}



********************************************************************************

"

introcomments2 <- stri_split(introcomments2, fixed = "\n")[[1]]
introcomments2 <- stri_c("// ", introcomments2) |> paste0(collapse = "\n")
cat(introcomments2)




################################################################################
# Odometer ====
#



macro_docall <- "

#define MACRO_DIM_DOCALL(DOCODE, GETX, GETY) do {     \\
  MACRO_DIM_ODOMETER(DOCODE, GETX, GETY);             \\
} while(0)"


macro_odometer <- "


#define MACRO_DIM_ODOMETER(DOCODE, GETX, GETY) do { \\
  \\
  /* Get const properties */  \\
  const int ndim = inline_determine_loopdepth(out_dim); \\
  const int *pout_dim = INTEGER_RO(out_dim);  \\
  const std::vector<R_xlen_t> alloc_stride_x = inline_alloc_strides(x_dim);  \\
  const std::vector<R_xlen_t> alloc_stride_y = inline_alloc_strides(y_dim);  \\
  const std::vector<int> alloc_N_bounds = inline_alloc_bounds(out_dim, ndim); \\
  const R_xlen_t outer_Niters = inline_determine_outer_Niters(alloc_N_bounds);  \\
  \\
  /* Declare variables */  \\
  R_xlen_t flatind_out = 0; \\
  R_xlen_t current_x = 0; \\
  R_xlen_t current_y = 0; \\
  int iters[16] = {0}; \\
  int N_bounds[16] = {0}; \\
  R_xlen_t stride_x[16] = {0}; \\
  R_xlen_t stride_y[16] = {0}; \\
  R_xlen_t contrib_x[16]= {0};  \\
  R_xlen_t contrib_y[16] = {0}; \\
  \\
  /*Initialise C-arrays */  \\
  const int N1 = pout_dim[0]; /* De bound voor de binnenste loop */ \\
  for (int d = 0; d < ndim; ++d) { \\
    N_bounds[d] = pout_dim[d]; \\
    stride_x[d] = alloc_stride_x[d]; \\
    stride_y[d] = alloc_stride_y[d]; \\
    N_bounds[d] = alloc_N_bounds[d]; \\
    contrib_x[d] = stride_x[d] * (N_bounds[d] - 1); \\
    contrib_y[d] = stride_y[d] * (N_bounds[d] - 1); \\
  } \\
  \\
  /* Outer-looop */ \\
  for (R_xlen_t outer = 0; outer < outer_Niters; ++outer) { \\
    R_xlen_t flatind_x = current_x; \\
    R_xlen_t flatind_y = current_y; \\
    const R_xlen_t stride_x1 = stride_x[0]; \\
    const R_xlen_t stride_y1 = stride_y[0]; \\
    /* Loop through rows */ \\
    for (int iter1 = 0; iter1 < N1; ++iter1) { \\
      GETX; \\
      GETY; \\
      DOCODE; \\
      \\
      flatind_out++; \\
      flatind_x += stride_x1; \\
      flatind_y += stride_y1; \\
    } \\
    \\
    odomacro(  \\
      ndim, iters, N_bounds, current_x, current_y, stride_x, stride_y, contrib_x, contrib_y \\
    ); \\
  } \\
} while(0)




"


odomacro <- "

#define odomacro(ndim, iters, N_bounds, current_x, current_y, stride_x, stride_y, contrib_x, contrib_y) do { \\
  for (int d = 1; d < ndim; ++d) {  \\
    iters[d]++;                     \\
    if (iters[d] < N_bounds[d]) {   \\
      current_x += stride_x[d];     \\
      current_y += stride_y[d];     \\
      break;                        \\
    } else {                        \\
      iters[d] = 0;                 \\
      current_x -= contrib_x[d];    \\
      current_y -= contrib_y[d];    \\
    }                               \\
  }                                 \\
} while(0)

"

alloc_strides <- "

inline std::vector<R_xlen_t> inline_alloc_strides(
  SEXP target_dim
) {
  
  // get properties:
  int n = Rf_length(target_dim);
  
  if(n > 16) {
    Rcpp::stop(\"dimensionality > 16 \");
  }
  
  std::vector<R_xlen_t> out(16);
  const int *ptarget = INTEGER_RO(target_dim);
  
  
  // make dcp:
  R_xlen_t temp_prod = ptarget[0];
  out[0] = 1;
  if(n > 1) {
    out[1] = ptarget[0];
  }
  if(n > 2) {
    for(int i = 2; i < n; ++i) {
      temp_prod = temp_prod * ptarget[i-1];
      out[i] = temp_prod;
    }
  }

  // mulitply with by (i.e. make it zero when appropriate):
  for(int i = 0; i < n; ++i) {
    if(ptarget[i] <= 1) {
      out[i] = 0;
    }
  }
  
  return out;
  
}
"


Rcpp::cppFunction(alloc_strides)


alloc_bounds <- "

inline std::vector<int> inline_alloc_bounds(
  SEXP out_dim, int ndim
) {

  const int *pout_dim = INTEGER_RO(out_dim);
  
  if(ndim > 16 || Rf_length(out_dim) > 16) {
    Rcpp::stop(\"dimensionality > 16 \");
  }
  
  std::vector<int> N_bounds(16);
  for (int d = 0; d < 16; ++d) {
    N_bounds[d] = 1;
  }
  
  for (int d = 0; d < ndim; ++d) {
    N_bounds[d] = pout_dim[d];
  }
  return N_bounds;
}

"

Rcpp::cppFunction(alloc_bounds)


determine_loopdepth <- "
inline int inline_determine_loopdepth(
  SEXP out_dim
) {
  
  if(Rf_length(out_dim) > 16) {
    Rcpp::stop(\"dimensionality > 16 \");
  }
  
  int n = Rf_length(out_dim);
  const int *pod = INTEGER_RO(out_dim);
  int rm = 0;
  for(int i = (n-1); i >= 0; --i) {
    if(pod[i] == 1) {
      rm++;
    }
    else {
      break;
    }
  }
  int loopdepth = n - rm;
  if(loopdepth == 0) {
   loopdepth = 1;
  }
  if(loopdepth > 16) {
    loopdepth = 16;
  }
  return loopdepth;
  
}

"

Rcpp::cppFunction(determine_loopdepth)

determine_outer_Niters <- "
inline R_xlen_t inline_determine_outer_Niters(std::vector<int> alloc_N_bounds) {
  // We multiply all alloc_N_bounds EXCEPT(!!!) the rows
  R_xlen_t outer_Niters = 1;
  int n = alloc_N_bounds.size();
  if(n == 1) return outer_Niters;
  
  for(int i = 1; i < n; ++i) {
    outer_Niters *= alloc_N_bounds[i];
  }
  return outer_Niters;
}

"

Rcpp::cppFunction(determine_outer_Niters)

# test inlines:
source(file.path(getwd(), "test", "test-inlines.R"))


odometer <- paste0(
  macro_docall,
  macro_odometer,
  odomacro,
  alloc_strides,
  alloc_bounds,
  determine_loopdepth,
  determine_outer_Niters,
  collapse = "\n"
)





################################################################################
# Intro 3 ====
#

introcomments3 <- "

********************************************************************************
MACROs for the binding implementation

The following MACROs define the loops used for broadcasted binding.

The MACROs were written for 16 dimensions.
These MACROs were written via a simple 'R' script,
to minimize the risk of human error.

********************************************************************************

"

introcomments3 <- stri_split(introcomments3, fixed = "\n")[[1]]
introcomments3 <- stri_c("// ", introcomments3) |> paste0(collapse = "\n")
cat(introcomments3)



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
  sprintf("R_xlen_t i_out%d = iter%d * pdcp_out[%d] + i_out%d;\t\\",
          2:16, 2:16, 1:15, 3:17)
)
forx <- c(
  "\t\\",
  sprintf("R_xlen_t i_x%d = pby_x[%d] * (iter%d - start%d) * pdcp_x[%d] + i_x%d;\t\\",
          2:16, 1:15, 2:16, 2:16, 1:15, 3:17)
)
all_for <- stri_c(all_for, forout, forx, sep = "\n")
cat(all_for[16])



temp <- "
#define MACRO_DIM_BIND_<dtype>(DOCODE) do {  \\
  const int *pby_x = INTEGER_RO(by_x);  \\
  const double *pdcp_out = REAL_RO(dcp_out);  \\
  const double *pdcp_x = REAL_RO(dcp_x);  \\
                                  \\
  <all_start_decl>
  <all_end_decl>
                                        \\
  <startfor>
        R_xlen_t flatind_out = iter1 + i_out2;       \\
        R_xlen_t flatind_x = pby_x[0] * (iter1 - start1) + i_x2;           \\
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
    "<startfor>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_start_decl,
    current_end_decl,
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

################################################################################
# Save macros ====
#

macro_dim <- stri_c(
  "\n",
  introcomments,
  "\n",
  macro_dim_twovectors,
  "\n",
  macro_dim_onevector,
  "\n",
  introcomments2,
  "\n",
  odometer,
  "\n",
  introcomments3,
  "\n",
  macro_dim_bind,
  "\n",
  macro_dim_bind_docall,
  "\n"
)

readr::write_file(macro_dim, "txt/macro_dim.txt")
