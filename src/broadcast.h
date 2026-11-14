

#ifndef BROADCAST_H
#define BROADCAST_H

#include <Rcpp/Lightest>


// 
// 
// ********************************************************************************
// INLINE FUNCTIONS
// 
// ********************************************************************************
// 
// 



#define DEF_INT53MAX std::pow(2, 53) - 1;

#define DEF_INT32MAX std::pow(2, 31) - 1;

inline bool inline_dims_is_vector(SEXP x) {
  
  if(x == R_NilValue) {
    return true;
  }
  
  const int n = Rf_xlength(x);
  const int *px = INTEGER_RO(x);
  int count_big = 0;
  
  for(int i = 0; i < n; ++i) {
    count_big += px[i] > 1;
  }
  
  
  return (count_big <= 1);
}


inline bool inline_branchless_or(bool x, bool y) {
  // This operation uses bit-wise `OR` since that is branchless and thus faster...
  // boolean `OR` uses short-circuit, but that's only faster if either side is a heavy computation
  
  // bit-wise `OR` only works properly like boolean `OR` if input is trictly 0 or 1 INTEGERS
  // therefore, input declared as (bool), and shortly before operation coerce to (int).
  
  return (int)x | (int)y;
}


inline double inline_mod_body(double x, double y) {
  double q = x / y;
  double tmp = x - floor(q) * y;
  double out = (double) (tmp - floorl(tmp / y) * y);
  return out;
}

inline double inline_idiv_body(double x, double y, double q) {
  if(std::abs(q) < 1) {
    double z = (q < 0) ? -1
    : ((x < 0 && y > 0) ||
       (x > 0 && y < 0) // differing signs
       ? -1 : 0);
    return z;
  }
  
  double tmp = (double)(x - floor(q) * (double)y);
  return (double) (floor(q) + floorl(tmp/y));
}

inline long long inline_gcd_body(long long x, long long y) {

  long long a = std::abs(x);
  long long b = std::abs(y);
  if(a == 0) return b;
  if(b == 0) return a;
  
  while (b != 0) {
    long long remainder = a % b;
    a = b;
    b = remainder;
  }
  
  return a;
  
}





// 
// 
// 
// ********************************************************************************
// MACROs for broadcasted binary operations where one or both sides are vectors
// 
// The following MACROs define the loops used for broadcasted element-wise binary operations,
// specifically where one or both sides are (oriented) vectors.
// 
// The dimensions of both arrays are first NORMALIZED and SIMPLIFIED virtually
// (see 'rcpp_virt_binary_prep' code),
// before determining which technique to use.
// 
// 
// ********************************************************************************
// 
// 


#define MACRO_DIM_VECTOR(DOCODE, GETX, GETY) do {                               \
  R_xlen_t flatind_x = 0;                                           \
  R_xlen_t flatind_y = 0;                                           \
  int by_x = 0;                                                     \
  int by_y = 0;                                                     \
  if(Rf_xlength(x) == Rf_xlength(y)) {                              \
    if(Rf_xlength(x) == 1) {                                        \
      by_x = 0;                                                     \
      by_y = 0;                                                     \
    }                                                               \
    else {                                                          \
      by_x = 1;                                                     \
      by_y = 1;                                                     \
    }                                                               \
  }                                                                 \
  if(Rf_xlength(x) != Rf_xlength(y)) {                              \
    if(Rf_xlength(x) == 1) {                                        \
      by_x = 0;                                                     \
      by_y = 1;                                                     \
    }                                                               \
    else if(Rf_xlength(y) ==1) {                                    \
      by_x = 1;                                                     \
      by_y = 0;                                                     \
    }                                                               \
  }                                                                 \
  for(R_xlen_t flatind_out = 0; flatind_out < nout; ++flatind_out) {  \
    GETX; \
    GETY; \
    DOCODE;                                                           \
    flatind_x += by_x;                                     \
    flatind_y += by_y;                                     \
  }                                                                   \
                                                                      \
} while(0)



#define MACRO_DIM_ORTHOVECTOR(DOCODE, GETX, GETY) do {      \
  R_xlen_t flatind_out = 0;         \
  const int N1 = INTEGER_RO(out_dim)[0];      \
  const int N2 = INTEGER_RO(out_dim)[1];       \
  bool RxC = INTEGER_RO(x_dim)[0] != 1; \
  if(RxC) { \
    for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {	\
      GETY; \
  	  for(int flatind_x = 0; flatind_x < N1; ++flatind_x) {	\
  	    GETX; \
        DOCODE;                         \
        ++flatind_out;                      \
    	 }	\
  	 }	\
  } \
  else {  \
    for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {	\
      GETX; \
    	  for(int flatind_y = 0; flatind_y < N1; ++flatind_y) {	\
    	    GETY; \
          DOCODE;                         \
          ++flatind_out;                      \
        }	\
    }	\
  } \
} while(0)



#define MACRO_DIM_TWOVECTORS(DOCODE, GETX, GETY) do {  \
  if(dimmode == 1) {  \
    MACRO_DIM_VECTOR(DOCODE, GETX, GETY); \
  } \
  else if(dimmode == 2) { \
    MACRO_DIM_ORTHOVECTOR(DOCODE, GETX, GETY);  \
  } \
  else {  \
    stop("dimmode is not a two-vector mode"); \
  } \
} while(0)






#define MACRO_DIM_BIG2VECTOR(DOCODE, GETX, GETY) do {      \
  const int N1 = INTEGER_RO(out_dim)[0];    \
  const int N2 = INTEGER_RO(out_dim)[1];    \
  const int N3 = INTEGER_RO(out_dim)[2];    \
  \
  if(vectorx) { \
    R_xlen_t flatind_y = 0;                                   \
    R_xlen_t flatind_out = 0;                                 \
    for(int iter3 = 0; iter3 < N3; ++iter3) {                 \
      for(int flatind_x = 0; flatind_x < N2; ++flatind_x) {   \
        GETX;                                                 \
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \
          GETY;                                               \
          DOCODE;                                             \
          ++flatind_y;                                        \
          ++flatind_out;                                      \
        }                                                     \
      }                                                       \
    }                                                         \
  } \
  else {  \
    R_xlen_t flatind_x = 0;                                   \
    R_xlen_t flatind_out = 0;                                 \
    for(int iter3 = 0; iter3 < N3; ++iter3) {                 \
      for(int flatind_y = 0; flatind_y < N2; ++flatind_y) {   \
        GETY;                                                 \
        for(int iter1 = 0; iter1 <N1; ++iter1) {              \
          GETX;                                               \
          DOCODE;                                             \
          ++flatind_x;                                        \
          ++flatind_out;                                      \
        }                                                     \
      }                                                       \
    }                                                         \
  } \
} while(0)


#define MACRO_DIM_SANDWICH2VECTOR(DOCODE, GETX, GETY) do {      \
  \
  if(vectorx) {  \
    R_xlen_t flatind_out = 0; \
    const int *pydim = INTEGER_RO(y_dim); \
    const R_xlen_t stride_y = (double)pydim[0] * (double)pydim[1];  \
    const R_xlen_t N1 = INTEGER_RO(out_dim)[0];  \
    const int N2 = INTEGER_RO(out_dim)[1];  \
    const R_xlen_t N3 = (R_xlen_t)INTEGER_RO(out_dim)[2] * stride_y;  \
    R_xlen_t flatind_y;\
    for(R_xlen_t iter3 = 0; iter3 < N3; iter3 += stride_y) {  \
      for(int flatind_x = 0; flatind_x < N2; ++flatind_x) { \
        GETX; \
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \
          flatind_y = iter3 + iter1; \
          GETY; \
          DOCODE; \
          ++flatind_out;  \
        } \
      } \
    } \
  } \
  else {  \
    R_xlen_t flatind_out = 0; \
    const int *pxdim = INTEGER_RO(x_dim); \
    const R_xlen_t stride_x = (double)pxdim[0] * (double)pxdim[1];  \
    const R_xlen_t N1 = INTEGER_RO(out_dim)[0];  \
    const int N2 = INTEGER_RO(out_dim)[1];  \
    const R_xlen_t N3 = INTEGER_RO(out_dim)[2] * stride_x;  \
    R_xlen_t flatind_x;\
    for(R_xlen_t iter3 = 0; iter3 < N3; iter3 += stride_x) {  \
      for(int flatind_y = 0; flatind_y < N2; ++flatind_y) { \
        GETY; \
        for(R_xlen_t iter1 = 0; iter1 < N1; ++iter1) {  \
          flatind_x = iter3 + iter1; \
          GETX; \
          DOCODE; \
          ++flatind_out;  \
        } \
      } \
    } \
  } \
} while(0)



#define MACRO_DIM_ONEVECTOR(DOCODE, GETX, GETY) do {  \
  bool vectorx = inline_dims_is_vector(x_dim);  \
  \
  if(dimmode == 3) { \
    MACRO_DIM_BIG2VECTOR(DOCODE, GETX, GETY); \
  } \
  else if(dimmode == 4) { \
    MACRO_DIM_SANDWICH2VECTOR(DOCODE, GETX, GETY); \
  } \
  else {  \
    stop("dimmode is not a one-vector mode"); \
  } \
} while(0)



// 
// 
// ********************************************************************************
// MACROs for the odometer
// 
// The following MACROs define the loops used for general broadcasted binary operations
// 
// The odometer itself works basically as follows:
// 
// 
// inline void odometer_step(
//   int ndim, int *iters, const int *N_bounds,
//   R_xlen_t *current_x, R_xlen_t *current_y,
//   const R_xlen_t *stride_x, const R_xlen_t *stride_y,
//   const R_xlen_t *contrib_x, const R_xlen_t *contrib_y
// ) {
//   
//   // basic relationship between flat index at one hand,
//   // and the dimensional iterations and strides at the other hand:
//   // flatind = iter1 * stride1 + iter2 * stride2 + etc.
//   
//   // at start, all iters are 0, thus flatind is 0:
//   // flatind = 0 + 0 + etc. == 0.
//   
//   // as we go through the rows, flatind increases with iter1 * stride1:
//   // flatind = iter1 * stride1
//   
//   // when all rows (d=0) have been passed,
//   // we need to add the contribution of going to the next column:
//   // (iter2 = 1) * stride2; this is simply +stride2
//   // AND we need to RESET the row's contribution,
//   // meaning we need to REMOVE what the rows have accumulated:
//   // remove `iter1 * stride1 * (N1 - 1)`
// 
//   // then we once again accumulate rows.
//   // everytime we've gone through all rows:
//   // - REMOVE the contribution to the rows; i.e. remove `iter1 * stride1 * (N1 - 1)`
//   // - we arleady have iter2=1; now we need iter2 = 2, so add +stride2 to the flatind.
//   
//   // and so we go on.
//   
//   // at some point, we've gone through all the columns and rows.
//   // then we do to the columns what we do with the rows:
//   // - remove the contribution of the columns
//   // - remove the contribution of the rows also
//   // - we need to add the contribution of going to the next layer:
//   //      (iter3 = 1) * stride3; this is simply +stride3
// 
//   // and so we continue
//   
//   // notice that after each cycle, flatind is pushed back by the stride of the current dimension.
//   // but flatind never gets pushed back further than the stride of one dimension.
//   // this, in a lighting-shape, we eventually keep moving forward, not backward.
//   // Like a cartesian product
// 
//     // We start at d = 1, since d=0 represents the rows
//     
//   for (int d = 1; d < ndim; ++d) {
//     
//     iters[d]++; // this always start with increasing the column iterator;
//     // whether we can even reach further dimensions depends on the if-else statements below...
//     
//     if (iters[d] < N_bounds[d]) {
//       // do the following if we have NOT gone through all iters of dimension `d`
//     
//       *current_x += stride_x[d]; // add contribution from column, layer, etc.
//       *current_y += stride_y[d]; // add contribution from column, layer, etc.
//       break; // done; do NOT run for the next `d`s
//         
//     } else {
//       // dimension `d` (column, layer, etc.) is done; time to....
//       iters[d] = 0; // reset iterator of current dimension (column, layer, etc.)
//       *current_x -= contrib_x[d]; // remove its contribution
//       *current_y -= contrib_y[d]; // remove its contribution
//         
//       // NO BREAK! we'll continue to d+1...
//     }
//   }
// }
// 
// 
// 
// ********************************************************************************
// 
// 


#define MACRO_DIM_DOCALL(DOCODE, GETX, GETY) do {     \
  MACRO_DIM_ODOMETER(DOCODE, GETX, GETY);             \
} while(0)


#define MACRO_DIM_ODOMETER(DOCODE, GETX, GETY) do { \
  \
  /* Get const properties */  \
  const int ndim = inline_determine_loopdepth(out_dim); \
  const int *pout_dim = INTEGER_RO(out_dim);  \
  const std::vector<R_xlen_t> alloc_stride_x = inline_alloc_strides(x_dim);  \
  const std::vector<R_xlen_t> alloc_stride_y = inline_alloc_strides(y_dim);  \
  const std::vector<int> alloc_N_bounds = inline_alloc_bounds(out_dim, ndim); \
  const R_xlen_t outer_Niters = inline_determine_outer_Niters(alloc_N_bounds);  \
  \
  /* Declare variables */  \
  R_xlen_t flatind_out = 0; \
  R_xlen_t current_x = 0; \
  R_xlen_t current_y = 0; \
  int iters[16] = {0}; \
  int N_bounds[16] = {0}; \
  R_xlen_t stride_x[16] = {0}; \
  R_xlen_t stride_y[16] = {0}; \
  R_xlen_t contrib_x[16]= {0};  \
  R_xlen_t contrib_y[16] = {0}; \
  \
  /*Initialise C-arrays */  \
  const int N1 = pout_dim[0]; /* De bound voor de binnenste loop */ \
  for (int d = 0; d < ndim; ++d) { \
    N_bounds[d] = pout_dim[d]; \
    stride_x[d] = alloc_stride_x[d]; \
    stride_y[d] = alloc_stride_y[d]; \
    N_bounds[d] = alloc_N_bounds[d]; \
    contrib_x[d] = stride_x[d] * (N_bounds[d] - 1); \
    contrib_y[d] = stride_y[d] * (N_bounds[d] - 1); \
  } \
  \
  /* Outer-looop */ \
  for (R_xlen_t outer = 0; outer < outer_Niters; ++outer) { \
    R_xlen_t flatind_x = current_x; \
    R_xlen_t flatind_y = current_y; \
    const R_xlen_t stride_x1 = stride_x[0]; \
    const R_xlen_t stride_y1 = stride_y[0]; \
    /* Loop through rows */ \
    for (int iter1 = 0; iter1 < N1; ++iter1) { \
      GETX; \
      GETY; \
      DOCODE; \
      \
      flatind_out++; \
      flatind_x += stride_x1; \
      flatind_y += stride_y1; \
    } \
    \
    odomacro(  \
      ndim, iters, N_bounds, current_x, current_y, stride_x, stride_y, contrib_x, contrib_y \
    ); \
  } \
} while(0)






#define odomacro(ndim, iters, N_bounds, current_x, current_y, stride_x, stride_y, contrib_x, contrib_y) do { \
  for (int d = 1; d < ndim; ++d) {  \
    iters[d]++;                     \
    if (iters[d] < N_bounds[d]) {   \
      current_x += stride_x[d];     \
      current_y += stride_y[d];     \
      break;                        \
    } else {                        \
      iters[d] = 0;                 \
      current_x -= contrib_x[d];    \
      current_y -= contrib_y[d];    \
    }                               \
  }                                 \
} while(0)



inline std::vector<R_xlen_t> inline_alloc_strides(
  SEXP target_dim
) {
  
  // get properties:
  int n = Rf_length(target_dim);
  
  if(n > 16) {
    Rcpp::stop("dimensionality > 16 ");
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


inline std::vector<int> inline_alloc_bounds(
  SEXP out_dim, int ndim
) {

  const int *pout_dim = INTEGER_RO(out_dim);
  
  if(ndim > 16 || Rf_length(out_dim) > 16) {
    Rcpp::stop("dimensionality > 16 ");
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


inline int inline_determine_loopdepth(
  SEXP out_dim
) {
  
  if(Rf_length(out_dim) > 16) {
    Rcpp::stop("dimensionality > 16 ");
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


// 
// 
// ********************************************************************************
// MACROs for the binding implementation
// 
// The following MACROs define the loops used for broadcasted binding.
// 
// The MACROs were written for 16 dimensions.
// These MACROs were written via a simple 'R' script,
// to minimize the risk of human error.
// 
// ********************************************************************************
// 
// 

#define MACRO_DIM_BIND_16(DOCODE) do {  \
  const int *pby_x = INTEGER_RO(by_x);  \
  const double *pdcp_out = REAL_RO(dcp_out);  \
  const double *pdcp_x = REAL_RO(dcp_x);  \
                                  \
  const int start1 = INTEGER_RO(starts)[0];	\
const int start2 = INTEGER_RO(starts)[1];	\
const int start3 = INTEGER_RO(starts)[2];	\
const int start4 = INTEGER_RO(starts)[3];	\
const int start5 = INTEGER_RO(starts)[4];	\
const int start6 = INTEGER_RO(starts)[5];	\
const int start7 = INTEGER_RO(starts)[6];	\
const int start8 = INTEGER_RO(starts)[7];	\
const int start9 = INTEGER_RO(starts)[8];	\
const int start10 = INTEGER_RO(starts)[9];	\
const int start11 = INTEGER_RO(starts)[10];	\
const int start12 = INTEGER_RO(starts)[11];	\
const int start13 = INTEGER_RO(starts)[12];	\
const int start14 = INTEGER_RO(starts)[13];	\
const int start15 = INTEGER_RO(starts)[14];	\
const int start16 = INTEGER_RO(starts)[15];	\
  const int end1 = INTEGER_RO(ends)[0];	\
const int end2 = INTEGER_RO(ends)[1];	\
const int end3 = INTEGER_RO(ends)[2];	\
const int end4 = INTEGER_RO(ends)[3];	\
const int end5 = INTEGER_RO(ends)[4];	\
const int end6 = INTEGER_RO(ends)[5];	\
const int end7 = INTEGER_RO(ends)[6];	\
const int end8 = INTEGER_RO(ends)[7];	\
const int end9 = INTEGER_RO(ends)[8];	\
const int end10 = INTEGER_RO(ends)[9];	\
const int end11 = INTEGER_RO(ends)[10];	\
const int end12 = INTEGER_RO(ends)[11];	\
const int end13 = INTEGER_RO(ends)[12];	\
const int end14 = INTEGER_RO(ends)[13];	\
const int end15 = INTEGER_RO(ends)[14];	\
const int end16 = INTEGER_RO(ends)[15];	\
                                        \
  	 for(int iter16 = start16; iter16 <= end16; ++iter16) {	\
R_xlen_t i_out16 = iter16 * pdcp_out[15];	\
R_xlen_t i_x16 = pby_x[15] * (iter16 - start16) * pdcp_x[15];	\
	 for(int iter15 = start15; iter15 <= end15; ++iter15) {	\
R_xlen_t i_out15 = iter15 * pdcp_out[14] + i_out16;	\
R_xlen_t i_x15 = pby_x[14] * (iter15 - start15) * pdcp_x[14] + i_x16;	\
	 for(int iter14 = start14; iter14 <= end14; ++iter14) {	\
R_xlen_t i_out14 = iter14 * pdcp_out[13] + i_out15;	\
R_xlen_t i_x14 = pby_x[13] * (iter14 - start14) * pdcp_x[13] + i_x15;	\
	 for(int iter13 = start13; iter13 <= end13; ++iter13) {	\
R_xlen_t i_out13 = iter13 * pdcp_out[12] + i_out14;	\
R_xlen_t i_x13 = pby_x[12] * (iter13 - start13) * pdcp_x[12] + i_x14;	\
	 for(int iter12 = start12; iter12 <= end12; ++iter12) {	\
R_xlen_t i_out12 = iter12 * pdcp_out[11] + i_out13;	\
R_xlen_t i_x12 = pby_x[11] * (iter12 - start12) * pdcp_x[11] + i_x13;	\
	 for(int iter11 = start11; iter11 <= end11; ++iter11) {	\
R_xlen_t i_out11 = iter11 * pdcp_out[10] + i_out12;	\
R_xlen_t i_x11 = pby_x[10] * (iter11 - start11) * pdcp_x[10] + i_x12;	\
	 for(int iter10 = start10; iter10 <= end10; ++iter10) {	\
R_xlen_t i_out10 = iter10 * pdcp_out[9] + i_out11;	\
R_xlen_t i_x10 = pby_x[9] * (iter10 - start10) * pdcp_x[9] + i_x11;	\
	 for(int iter9 = start9; iter9 <= end9; ++iter9) {	\
R_xlen_t i_out9 = iter9 * pdcp_out[8] + i_out10;	\
R_xlen_t i_x9 = pby_x[8] * (iter9 - start9) * pdcp_x[8] + i_x10;	\
	 for(int iter8 = start8; iter8 <= end8; ++iter8) {	\
R_xlen_t i_out8 = iter8 * pdcp_out[7] + i_out9;	\
R_xlen_t i_x8 = pby_x[7] * (iter8 - start8) * pdcp_x[7] + i_x9;	\
	 for(int iter7 = start7; iter7 <= end7; ++iter7) {	\
R_xlen_t i_out7 = iter7 * pdcp_out[6] + i_out8;	\
R_xlen_t i_x7 = pby_x[6] * (iter7 - start7) * pdcp_x[6] + i_x8;	\
	 for(int iter6 = start6; iter6 <= end6; ++iter6) {	\
R_xlen_t i_out6 = iter6 * pdcp_out[5] + i_out7;	\
R_xlen_t i_x6 = pby_x[5] * (iter6 - start6) * pdcp_x[5] + i_x7;	\
	 for(int iter5 = start5; iter5 <= end5; ++iter5) {	\
R_xlen_t i_out5 = iter5 * pdcp_out[4] + i_out6;	\
R_xlen_t i_x5 = pby_x[4] * (iter5 - start5) * pdcp_x[4] + i_x6;	\
	 for(int iter4 = start4; iter4 <= end4; ++iter4) {	\
R_xlen_t i_out4 = iter4 * pdcp_out[3] + i_out5;	\
R_xlen_t i_x4 = pby_x[3] * (iter4 - start4) * pdcp_x[3] + i_x5;	\
	 for(int iter3 = start3; iter3 <= end3; ++iter3) {	\
R_xlen_t i_out3 = iter3 * pdcp_out[2] + i_out4;	\
R_xlen_t i_x3 = pby_x[2] * (iter3 - start3) * pdcp_x[2] + i_x4;	\
	 for(int iter2 = start2; iter2 <= end2; ++iter2) {	\
R_xlen_t i_out2 = iter2 * pdcp_out[1] + i_out3;	\
R_xlen_t i_x2 = pby_x[1] * (iter2 - start2) * pdcp_x[1] + i_x3;	\
	 for(int iter1 = start1; iter1 <= end1; ++iter1) {	\
	\
	\
        R_xlen_t flatind_out = iter1 + i_out2;       \
        R_xlen_t flatind_x = pby_x[0] * (iter1 - start1) + i_x2;           \
        DOCODE;                         \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_BIND_DOCALL(DOCODE) do {     \
  int ndims = Rf_length(out_dim);         \
                                          \
  switch(ndims) {       \
    case 16:                                       \
  MACRO_DIM_BIND_16(DOCODE);    \
  break;                                        \
       \
  }       \
} while(0)



#endif
