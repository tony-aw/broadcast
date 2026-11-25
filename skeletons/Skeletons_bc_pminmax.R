# set-up ====

library(stringi)
source("SOURCE.R")

################################################################################
# MACROS ====
#

macro_action_pminmax <- "

#define MACRO_ACTION_PMINMAX_INTEGER(DOCODE) do {   \\
  if(inline_branchless_or(tempx == NA_INTEGER, tempy == NA_INTEGER)) {  \\
    pout[flatind_out] = NA_INTEGER; \\
  } \\
  else {  \\
    pout[flatind_out] = DOCODE;  \\
  } \\
} while(0)


#define MACRO_ACTION_PMINMAX_REAL(DOCODE) do {   \\
  if(inline_branchless_or(std::isnan(tempx), std::isnan(tempy))) {  \\
    pout[flatind_out] = NA_REAL; \\
  } \\
  else {  \\
    pout[flatind_out] = DOCODE;  \\
  } \\
} while(0)


"

macro_typeswitch_pminmax <- "

#define MACRO_TYPESWITCH_PMINMAX(DIMCODE, DOCODE) do { \\
    bool isInt_x = TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP;    \\
    bool isInt_y = TYPEOF(y) == LGLSXP || TYPEOF(y) == INTSXP;    \\
                                                                  \\
    if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {            \\
      NumericVector out(no_init(nout)); \\
      double *pout = REAL(out);         \\
      const double *px = REAL_RO(x);    \\
      const double *py = REAL_RO(y);    \\
      DIMCODE(                                          \\
        MACRO_ACTION_PMINMAX_REAL(                           \\
          DOCODE                                        \\
        ),                                              \\
        const double tempx = px[flatind_x],    \\
        const double tempy = py[flatind_y]     \\
      );                                                \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                   \\
    else if(isInt_x && isInt_y) {       \\
      IntegerVector out(no_init(nout)); \\
      int *pout = INTEGER(out);         \\
      const int *px = INTEGER_RO(x);    \\
      const int *py = INTEGER_RO(y);    \\
      DIMCODE(                                          \\
        MACRO_ACTION_PMINMAX_INTEGER(                           \\
          DOCODE                                        \\
        ),                                              \\
        const int tempx = px[flatind_x],    \\
        const int tempy = py[flatind_y]     \\
      );                                                \\
      out.attr(\"dim\") = out_dimorig;  \\
      return out;                       \\
    }                                                   \\
    else {                                                        \\
      stop(\"unsupported types given\");        \\
    }                                           \\
} while(0)


"




macro_op_pminmax <- "
#define MACRO_OP_PMINMAX(DIMCODE) do { \\
  switch(op) {	\\
    case 1:	\\
    {	\\
      MACRO_TYPESWITCH_PMINMAX(    \\
        DIMCODE,	        \\
        tempx < tempy ? tempx : tempy     \\
      );	                              \\
      break;	\\
    }	\\
    case 2:	\\
    {	\\
      MACRO_TYPESWITCH_PMINMAX(    \\
        DIMCODE,	        \\
        tempx > tempy ? tempx : tempy     \\
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


macros_pminmax <- paste0(macro_action_pminmax, macro_typeswitch_pminmax, macro_op_pminmax)
Rcpp::sourceCpp(code = paste0(header_for_sourcing, macros_pminmax))


################################################################################
# Functions ====
#


txt1 <- "


//' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_pminmax_2v, rng = false)]]
  SEXP rcpp_bc_pminmax_2v(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  int dimmode = prep[5];          (void)dimmode;

  
  MACRO_OP_PMINMAX(MACRO_DIM_TWOVECTORS);
  return R_NilValue;
  }
  
  
  
  
  //' @keywords internal
  //' @noRd
  // [[Rcpp::export(.rcpp_bc_pminmax_d, rng = false)]]
  SEXP rcpp_bc_pminmax_d(
    SEXP x, SEXP y, List prep, int op 
  ) {
  
  
  RObject x_dim = prep[0];        (void)x_dim;
  RObject y_dim = prep[1];        (void)y_dim;
  RObject out_dimorig = prep[2];  (void)out_dimorig;
  RObject out_dim = prep[3];      (void)out_dim;
  R_xlen_t nout = prep[4];        (void)nout;
  
  
  MACRO_OP_PMINMAX(MACRO_DIM_DOCALL);
  return R_NilValue;
  
  
  
  }
  


"


txt <- stringi::stri_c(
  header_for_sourcing,
  macros_pminmax,
  txt1,
  collapse = "\n\n"
)
Rcpp::sourceCpp(code = txt)

setwd("..")
txt <- stringi::stri_c(
  header_for_package,
  macros_pminmax,
  txt1,
  collapse = "\n\n"
)
readr::write_file(txt, "src/rcpp_bc_pminmax.cpp")

