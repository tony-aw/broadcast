

#include <Rcpp.h>
#include "Macros_Everywhere.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_v)]]
SEXP rcpp_bc_dbl_v(
  SEXP x, SEXP y,
  R_xlen_t nout, int op
) {

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_VECTOR,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_VECTOR,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_bs)]]
SEXP rcpp_bc_dbl_bs(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, bool bigx,
  int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_BIGSMALL_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_BIGSMALL_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_o)]]
SEXP rcpp_bc_dbl_o(
  SEXP x, SEXP y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, bool xstarts,
  int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_ORTHO_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_ORTHO_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_d)]]
SEXP rcpp_bc_dbl_d(
  SEXP x, SEXP y,
  SEXP by_x,
  SEXP by_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y, SEXP out_dim, R_xlen_t nout, int op
) {


double *pdcp_x = REAL(dimcumprod_x);
double *pdcp_y = REAL(dimcumprod_y);

double tempout;

SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
double *pout;
pout = REAL(out);

switch(op) {
  case 1:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] + (double)py[flatind_y]
    );
    break;
  }
  case 2:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] - (double)py[flatind_y]
    );
    break;
  }
  case 3:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] * (double)py[flatind_y]
    );
    break;
  }
  case 4:
  {
    MACRO_TYPESWITCH_NUMERIC_COMMON(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = (double)px[flatind_x] / (double)py[flatind_y]
    );
    break;
  }
  case 5:
  {
    MACRO_TYPESWITCH_NUMERIC_SPECIAL(
      MACRO_DIM_DOCALL,
      (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
      tempout = 1,
      tempout = NA_REAL,
      tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
    );
    break;
  }
  case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_DOCALL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  default:
  {
    stop("given operator not supported in the given context");
  }
}

UNPROTECT(1);
return out;

}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_bc_dbl_general)]]
SEXP rcpp_bc_dbl_general(
  SEXP x, SEXP y,
  const SEXP s1, const SEXP s2,
  const SEXP xdims1, const SEXP xdims2, const R_xlen_t nout, int op
) {
  
  double tempout;

  SEXP out = PROTECT(Rf_allocVector(REALSXP, nout));
  double *pout;
  pout = REAL(out);
  
  switch(op) {
    case 1:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] + (double)py[flatind_y]
      );
      break;
    }
    case 2:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] - (double)py[flatind_y]
      );
      break;
    }
    case 3:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] * (double)py[flatind_y]
      );
      break;
    }
    case 4:
    {
      MACRO_TYPESWITCH_NUMERIC_COMMON(
        MACRO_DIM_GENERAL,
        tempout = NA_REAL,
        tempout = (double)px[flatind_x] / (double)py[flatind_y]
      );
      break;
    }
    case 5:
    {
      MACRO_TYPESWITCH_NUMERIC_SPECIAL(
        MACRO_DIM_GENERAL,
        (double)px[flatind_x] == 1 || (double)py[flatind_y] == 0,
        tempout = 1,
        tempout = NA_REAL,
        tempout = R_pow((double)px[flatind_x], (double)py[flatind_y])
      );
      break;
    }
    case 6:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] < (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
  case 7:
  {
    MACRO_TYPESWITCH_NUMERIC_CAREFUL(
      MACRO_DIM_GENERAL,
      tempout = NA_REAL,
      tempout = ((double)px[flatind_x] > (double)py[flatind_y]) ? (double)px[flatind_x] : (double)py[flatind_y] 
    );
    break;
  }
    default:
    {
      stop("given operator not supported in the given context");
    }
  
  }
  
  
  UNPROTECT(1);
  return out;
  
  
}


