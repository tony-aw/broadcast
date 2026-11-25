# set-up ====

library(stringi)
source("SOURCE.R")



################################################################################
# Inlines ====
#

inlines <- "
  inline int inline_int32_plus(long long x, long long y, const int intmin, const int intmax, bool &warn) {
    if(x == NA_INTEGER || y == NA_INTEGER) {
      return NA_INTEGER;
    }
    const long long out = x + y;
    if(out > intmax || out < intmin) {
      
    }
  }

"


################################################################################
# MACROS ====
#

macro_action_plusmin <- "

#define MACRO_ACTION_PLUSMIN(DOCODE) do {   \\
  if(tempx == NA_INTEGER || tempy == NA_INTEGER) {  \\
    pout[flatind_out] = NA_INTEGER; \\
  } \\
  else {  \\
    const long long tempout = DOCODE; \\
    if(tempout > intmax || tempout < intmin) {  \\
      warn = 1; \\
      pout[flatind_out] = NA_INTEGER;  \\
    } \\
    else {                          \\
      pout[flatind_out] = (int) tempout;  \\
    }                               \\
  } \\
} while(0)
"

macro_typeswitch_plusmin <- "

#define MACRO_TYPESWITCH_PLUSMIN(DIMCODE, DOCODE) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      NumericVector out(no_init(nout)); \\
      double *pout = REAL(out);         \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                          \\
        pout[flatind_out] = DOCODE,    \\
        const double tempx = px[flatind_x], \\
        const double tempy = py[flatind_y]  \\
      );                                \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                   \\
    else if(isInt_x && isInt_y) {       \\
     bool warn = 0;                     \\
      double intmax = DEF_INT32MAX;     \\
      double intmin = -1 * intmax;      \\
      IntegerVector out(no_init(nout)); \\
      int *pout = INTEGER(out);         \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                                          \\
        MACRO_ACTION_PLUSMIN(                           \\
          DOCODE                                        \\
        ),                                              \\
        const long long tempx = (long long)px[flatind_x],    \\
        const long long tempy = (long long)py[flatind_y]     \\
      );                                                \\
      if(warn) INTEGER(warn0)[0] = 1;  \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)


"




macro_op_plusmin <- "
#define MACRO_OP_PLUSMIN(DIMCODE) do { \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_PLUSMIN(    \\
        DIMCODE,	        \\
        tempx + tempy     \\
      );	                              \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_PLUSMIN(    \\
        DIMCODE,	        \\
        tempx - tempy     \\
      );	                              \\
      break;	\\
    }	\\
    default:	\\
    {	\\
      stop(\"given operator not supported in the given context\");	\\
    }	\\
  }	\\
} while(0)
"


macros_plusmin <- paste0(macro_action_plusmin, macro_typeswitch_plusmin, macro_op_plusmin)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_plusmin))


################################################################################
# Functions ====
#


txt1 <- "


//' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_num_plusmin_2v, rng = false)]]
  SEXP rcpp_bc_num_plusmin_2v(
    SEXP x, SEXP y, List prep, int op, SEXP warn0
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

  
  MACRO_OP_PLUSMIN(MACRO_DIM_TWOVECTORS);
  return R_NilValue;
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_num_plusmin_d, rng = false)]]
  SEXP rcpp_bc_num_plusmin_d(
    SEXP x, SEXP y, List prep, int op, SEXP warn0
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  MACRO_OP_PLUSMIN(MACRO_DIM_DOCALL);
  return R_NilValue;
  
  
  
  }
  


"


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_plusmin,
  txt1,
  collapse = "\n\n"
)
Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_plusmin,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_num_plusmin.cpp")

