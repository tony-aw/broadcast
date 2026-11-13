

#include <Rcpp/Lightest>
#include "broadcast.h"

using namespace Rcpp;




inline int inline_bool_AND(
  int x, int y
) {
  bool xFALSE = x == 0;
  bool yFALSE = y == 0;
  if(xFALSE || yFALSE) {
    return 0;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x && (bool)y);
  }
}

inline int inline_bool_OR(
  int x, int y
) {
  bool xTRUE = x != NA_INTEGER && x != 0;
  bool yTRUE = y != NA_INTEGER && y != 0;
  if(xTRUE || yTRUE) {
    return 1;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x || (bool)y);
  }
}


inline int inline_bool_XOR(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x != bool(y));
  }
}

inline int inline_bool_XNOR(
  int x, int y
) {
  if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    return ((bool)x == bool(y));
  }
}

inline int inline_bool_NAND(
  int x, int y
) {
  bool xFALSE = x == 0;
  bool yFALSE = y == 0;
  if(xFALSE || yFALSE) {
    return 1;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    int out = !((bool)x && (bool)y);
    return out;
  }
}


inline int inline_bool_NOR(
  int x, int y
) {
  bool xTRUE = x != NA_INTEGER && x != 0;
  bool yTRUE = y != NA_INTEGER && y != 0;
  if(xTRUE || yTRUE) {
    return 0;
  }
  else if(x == NA_INTEGER || y == NA_INTEGER) {
    return NA_LOGICAL;
  }
  else {
    int out = !((bool)x || bool(y));
    return out;
  }
}




#define MACRO_ACTION_BOOL_INT(DIMCODE, DOCODE) do {     \
  DIMCODE(                                              \
    pout[flatind_out] = DOCODE,                         \
    const int tempx = px[flatind_x],                    \
    const int tempy = py[flatind_y]                     \
  );                                                    \
} while(0)


#define MACRO_OP_BOOL_INT(DIMCODE) do {	\
  switch(op) {	\
    case 1:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_AND(tempx, tempy));  \
      break;	\
    }	\
    case 2:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_OR(tempx, tempy));  \
      break;	\
    }	\
    case 3:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XOR(tempx, tempy));  \
      break;	\
    }	\
    case 4:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_NAND(tempx, tempy));  \
      break;	\
    }	\
    case 5:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_NOR(tempx, tempy));  \
      break;	\
    }	\
    case 6:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XNOR(tempx, tempy));  \
      break;	\
    }	\
    case 7:	\
    {	\
      MACRO_ACTION_BOOL_INT(DIMCODE, inline_bool_XOR(tempx, tempy));  \
      break;	\
    }	\
    default:	\
    {	\
      stop("given operator not supported in the given context");	\
    }	\
  }	\
} while(0)

  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_b_int_2v, rng = false)]]
  SEXP rcpp_bc_b_int_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = LOGICAL(out); 

  
  MACRO_OP_BOOL_INT(MACRO_DIM_TWOVECTORS);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_b_int_1v, rng = false)]]
  SEXP rcpp_bc_b_int_1v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = LOGICAL(out); 

  
  MACRO_OP_BOOL_INT(MACRO_DIM_ONEVECTOR);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_b_int_d, rng = false)]]
  SEXP rcpp_bc_b_int_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  LogicalVector out(no_init(nout));
  
  
const int *px = INTEGER_RO(x);
const int *py = INTEGER_RO(y);
int *pout = LOGICAL(out); 

  
  MACRO_OP_BOOL_INT(MACRO_DIM_DOCALL);
  
  out.attr("dim") = out_dimorig;
  
  return out;
  
  }
  
  
  