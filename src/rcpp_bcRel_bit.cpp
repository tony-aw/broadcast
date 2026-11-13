
#include <Rcpp/Lightest>
#include "broadcast.h"
using namespace Rcpp;

#define MACRO_ACTION_BIT_REL_INT(DOCODE) do {       \
  if(tempx == NA_INTEGER || tempy == NA_INTEGER) {  \
    pout[flatind_out] = NA_INTEGER;                 \
  }                                                 \
  else {                                            \
    pout[flatind_out] = DOCODE;                     \
  }                                                 \
} while(0)



#define MACRO_OP_BIT_REL_RAW(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = ~(tempx ^ tempy), \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    case 2:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = tempx ^ tempy, \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    case 3:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = (~tempx & tempy), \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    case 4:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = (tempx & ~tempy), \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    case 5:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = (~tempx | tempy), \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    case 6:	\
    {	\
      DIMCODE(  \
        pout[flatind_out] = (tempx | ~tempy), \
        const Rbyte tempx = px[flatind_x],    \
        const Rbyte tempy = py[flatind_y]     \
      );                                                                \
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  } \
} while(0)



#define MACRO_OP_BIT_REL_INT(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          ~(tempx ^ tempy) \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    case 2:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          tempx ^ tempy \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    case 3:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          (~tempx & tempy) \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    case 4:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          (tempx & ~tempy) \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    case 5:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          (~tempx | tempy) \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    case 6:	\
    {	\
      DIMCODE(  \
        MACRO_ACTION_BIT_REL_INT( \
          (tempx | ~tempy) \
        ),  \
        const int tempx = px[flatind_x],  \
        const int tempy = py[flatind_y]   \
      );  \
      break;  \
    } \
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  } \
} while(0)



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_2v, rng = false)]]
SEXP rcpp_bcRel_bit_2v(
  SEXP x, SEXP y, List prep, int op
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;

  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    RawVector out(no_init(nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_TWOVECTORS);
    
    out.attr("dim") = out_dimorig;
    
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    IntegerVector out(no_init(nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_TWOVECTORS);
    
    out.attr("dim") = out_dimorig;
    
    return out;
    
  }
  else {
    stop("unsupported combinations of types given");
  }

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_1v, rng = false)]]
SEXP rcpp_bcRel_bit_1v(
  SEXP x, SEXP y, List prep, int op
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;

  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    RawVector out(no_init(nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_ONEVECTOR);
    
    out.attr("dim") = out_dimorig;
    
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    IntegerVector out(no_init(nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_ONEVECTOR);
    
    out.attr("dim") = out_dimorig;
    
    return out;
    
  }
  else {
    stop("unsupported combinations of types given");
  }
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bcRel_bit_d, rng = false)]]
SEXP rcpp_bcRel_bit_d(
  SEXP x, SEXP y, List prep, int op
) {

RObject x_dim = prep[0];        (void)x_dim;
RObject y_dim = prep[1];        (void)y_dim;
RObject out_dimorig = prep[2];  (void)out_dimorig;
RObject out_dim = prep[3];      (void)out_dim;
R_xlen_t nout = prep[4];        (void)nout;
int dimmode = prep[5];          (void)dimmode;

  
  if(TYPEOF(x) == RAWSXP && TYPEOF(y) == RAWSXP) {
    
    RawVector out(no_init(nout));
    Rbyte *pout = RAW(out);
    const Rbyte *px = RAW(x);
    const Rbyte *py = RAW(y);
    
    MACRO_OP_BIT_REL_RAW(MACRO_DIM_DOCALL);
    
    out.attr("dim") = out_dimorig;
    
    return out;
  }
  else if(TYPEOF(x) == INTSXP && TYPEOF(y) == INTSXP) {
    IntegerVector out(no_init(nout));
    int *pout = INTEGER(out);
    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    
    MACRO_OP_BIT_REL_INT(MACRO_DIM_DOCALL);
    
    out.attr("dim") = out_dimorig;
    
    return out;
    
  }
  else {
    stop("unsupported combinations of types given");
  }

}


