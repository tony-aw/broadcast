# set-up ====

library(stringi)
dMacro_skeletons <- qs::qread("dMacro_skeletons")
macros <- stri_c(dMacro_skeletons[2], collapse = "\n")


header <- stri_c("

#include <Rcpp.h>

using namespace Rcpp;
",
  macros,
  "\n",
  typeMacro_numeric
)




################################################################################
# dfunction skeleton ====
#

DTYPES <- c(2:8, 16)
all_args_ind_x <- stri_c("const SEXP ind", 1:16, "_x")
all_args_ind_y <- stri_c("const SEXP ind", 1:16, "_y")
setlengths <- paste("int len", 1:16, " = Rf_length(ind", 1:16, "_x);", sep= "")
makepointers_ind_x <- sprintf("const int *pind%d_x;\npind%d_x = INTEGER(ind%d_x);\n", 1:16, 1:16, 1:16)
makepointers_ind_y <- sprintf("const int *pind%d_y;\npind%d_y = INTEGER(ind%d_y);\n", 1:16, 1:16, 1:16)
all_lengths <- paste("len", 1:16, sep = "")

templatecode_fun <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_<bcname>_<output_type>_<dtype>d)]]
SEXP rcpp_<bcname>_<output_type>_<dtype>d(
  SEXP x, SEXP y,
  <args_ind_x>,
  <args_ind_y>,
  SEXP dimcumprod_x, SEXP dimcumprod_y <, additional_args>
) {



<starting_statements>


<setlengths>
<makepointers_ind_x>
<makepointers_ind_y>

int n = <length_mult>;

double *pdim_x;
pdim_x = REAL(dimcumprod_x);
double *pdim_y;
pdim_y = REAL(dimcumprod_y);

R_xlen_t flatind_x;
R_xlen_t flatind_y;

<output_type> tempout;

<specify_output>
  
if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {
  double *px = REAL(x);
  double *py = REAL(y);
  
  NUMERICIFELSE(
    BROADCASTLOOP_<dtype>(
      <nacheck>,
      <nacode>,
      <docode>
    )
  );
}
else {
  stop(\"unsupported combination of types given\");
}


<closing_statements>

return out;

}


"

rcpp_scripts <- character(length(DTYPES))
names(rcpp_scripts) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_args_ind_x <- stri_c(all_args_ind_x[1:i], collapse = ", ")
  current_args_ind_y <- stri_c(all_args_ind_y[1:i], collapse = ", ")
  current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
  current_makepointers_ind_x <- stri_c(makepointers_ind_x[1:i], collapse = "\n")
  current_makepointers_ind_y <- stri_c(makepointers_ind_y[1:i], collapse = "\n")
  current_length_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_macro <- dMacro_skeletons[as.character(i)]
  
  current_fixed <- c(
    "<dtype>",
    "<args_ind_x>",
    "<args_ind_y>",
    "<setlengths>",
    "<makepointers_ind_x>",
    "<makepointers_ind_y>",
    "<length_mult>",
    "<define_macro>"
  )
  current_replacement <- c(
    i,
    current_args_ind_x,
    current_args_ind_y,
    current_setlengths,
    current_makepointers_ind_x,
    current_makepointers_ind_y,
    current_length_mult,
    current_macro
  )
  
  out <- stri_replace_all(
    templatecode_fun,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  rcpp_scripts[[counter]] <- out
  counter <- counter + 1
}

cat(rcpp_scripts[[8]])

txt_dfunction_skeleton <- stringi::stri_c(rcpp_scripts[[2]], collapse =  "\n\n\n")
cat(txt_dfunction_skeleton)

write(txt_dfunction_skeleton, "dfunction_skeleton.txt")



################################################################################
# docall skeleton ====
#

# declare inds:
declare_inds_x <- stringi::stri_c(sprintf("SEXP ind%d_x;", 3:16), collapse = "\n")
declare_inds_y <- stringi::stri_c(sprintf("SEXP ind%d_y;", 3:16), collapse = "\n")


# ifelse inds:
ifelse_inds <- "
if(n > %d) {
  ind%d_x = sub_x[%d];
  ind%d_y = sub_y[%d];
"
ifelse_inds <- sprintf(ifelse_inds, 2:15, 3:16, 2:15, 3:16, 2:15) |>
  stringi::stri_c(collapse = "\n")
ifelse_close <- stri_c(rep("} \n", 14)) |> stringi::stri_c(collapse = "")


# cases:
case <- "
case %d:
  out = rcpp_<op>_<lhs_type>_<rhs_type>_<output_type>_%dd(
    x, y,
    <args%d_x>,
    <args%d_y>
    dimcumprod_x, dimcumprod_y<, additional_args>
  );
  break;
"
cases <- sprintf(case, 2:16, 2:16, 2:16, 2:16) |> stringi::stri_c(collapse = "\n")

args_x <- sprintf("ind%d_x", 1:16)
args_x <- sapply(2:16, \(i) stri_c(args_x[1:i], collapse = ", "))
cases <- stri_replace_all_fixed(
  cases,
  sprintf("<args%d_x>", 2:16),
  args_x,
  vectorize_all = FALSE
)

args_y <- sprintf("ind%d_y", 1:16)
args_y <- sapply(2:16, \(i) stri_c(args_y[1:i], collapse = ", "))
cases <- stri_replace_all_fixed(
  cases,
  sprintf("<args%d_y>", 2:16),
  args_y,
  vectorize_all = FALSE
)


cat(cases)

templatecode_docall <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_<op>_<lhs_type>_<rhs_type>_<output_type>_docall)]]
SEXP rcpp_<op>_<lhs_type>_<rhs_type>_<output_type>_docall(
  SEXP x, SEXP y, List sub_x, List sub_y,
  SEXP dimcumprod_x, SEXP dimcumprod_y <, additional_args>
) {
  
  int n = sub_x.length();
  
  SEXP ind1_x = sub_x[0];
  SEXP ind1_y = sub_y[0];
  SEXP ind2_x = sub_x[1];
  SEXP ind2_y = sub_y[1];

  <declare_inds_x>
  
  <declare_inds_y>
  
  SEXP out;

  <ifelse_inds>
  <ifelse_close>
  
  switch(n) {
    <cases>
  }
  
  return out;
}



"

templatecode_docall2 <- stringi::stri_replace_all(
  templatecode_docall,
  fixed = c("<declare_inds_x>", "<declare_inds_y>", "<ifelse_inds>", "<ifelse_close>", "<cases>"),
  replacement = c(declare_inds_x, declare_inds_y, ifelse_inds, ifelse_close, cases),
  vectorize_all = FALSE
)


cat(templatecode_docall2)


txt_docall_skeleton <- templatecode_docall2

write(txt_docall_skeleton, "docall_skeleton.txt")





################################################################################
# gfunction skeleton ====
#

# THIS WORKS :-)

txt_gfunction_skeleton <- "


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_<op>_<lhs_type>_<rhs_type>_<output_type>_general)]]
SEXP rcpp_<op>_<lhs_type>_<rhs_type>_<output_type>_general(
  SEXP x, SEXP y,
  const SEXP s1, const SEXP s2,
  const SEXP xdims1, const SEXP xdims2, const R_xlen_t nout
) {

  int k;
  
  const void *vmaxsave = vmaxget(); // Because I'm gonna use R_alloc()
  
  // s1 and s2 are lists
 
  k = Rf_length(xdims1);
  
  
  // allocate vectors (kinda like a list) through R_alloc()
  // k = ndims(x) = ndims(y)
  
  int **subs1 = (int**)R_alloc(k, sizeof(int*));
  int *indx1 = (int*)R_alloc(k, sizeof(int));
  int *bound1 = (int*)R_alloc(k, sizeof(int));
  R_xlen_t *offset1 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));
  
  int **subs2 = (int**)R_alloc(k, sizeof(int*));
  int *indx2 = (int*)R_alloc(k, sizeof(int));
  int *bound2 = (int*)R_alloc(k, sizeof(int));
  R_xlen_t *offset2 = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));
  
  
  // initiate allocated vectors
  
  R_xlen_t n1 = 1;
  SEXP r1;
  R_xlen_t n2 = 1;
  SEXP r2;
  
  for (int i = 0; i < k; i++) {
    r1 = VECTOR_ELT(s1, i); 
  	indx1[i] = 0;
  	bound1[i] = Rf_length(r1);
    n1 *= bound1[i];
  	subs1[i] = INTEGER(r1);
  	
  	r2 = VECTOR_ELT(s2, i); 
  	indx2[i] = 0;
  	bound2[i] = Rf_length(r2);
    n2 *= bound2[i];
  	subs2[i] = INTEGER(r2);
  }
  
  offset1[0] = 1;
  offset2[0] = 1;
  for (int i = 1; i < k; i++) {
    offset1[i] = offset1[i - 1] * INTEGER(xdims1)[i - 1];
    offset2[i] = offset2[i - 1] * INTEGER(xdims2)[i - 1];
  }
  
  
  // initiate input and output
  <output_type> tempout;

  <specify_input>

  <specify_output>
  
  
  
  // main loop
  
  R_xlen_t counter = 0;

  for (R_xlen_t i = 0; i < nout; i++) {
  
    R_xlen_t flatind_x = 0;
    R_xlen_t flatind_y = 0;
    
  	for (int j = 0; j < k; j++) {
	    int jj1 = subs1[j][indx1[j]];
	    flatind_x += (jj1 - 1) * offset1[j];
	    int jj2 = subs2[j][indx2[j]];
	    flatind_y += (jj2 - 1) * offset2[j];
  	}
  	
  	
  	// meat in sandwich
  	
  	<do_something>
  	
  	pout[counter] = tempout;
    counter++;
  	
	
	  int j1 = 0;
    while (++indx1[j1] >= bound1[j1]) {
  		indx1[j1] = 0;
  		j1 = (j1 + 1) % k;
    }
  	int j2 = 0;
    while (++indx2[j2] >= bound2[j2]) {
  		indx2[j2] = 0;
  		j2 = (j2 + 1) % k;
    }
	}
  
  

  // Free temporary memory
  vmaxset(vmaxsave);
  
  <closing_statements>
  
  return(out);
  
}


"


################################################################################
# make test function ====
#


cat(txt_dfunction_skeleton)

txt_plus <- txt_dfunction_skeleton
txt_plus <- stringi::stri_replace_all(
  txt_plus,
  fixed = c(
    "<bcname>", "<output_type>", "<, additional_args>",
    "<nacheck>", "<nacode>"
  ),
  replacement = c(
    "plus", "double", "",
    "R_isnancpp(px[flatind_x]) || R_isnancpp(py[flatind_y])", "tempout = NA_REAL"
  ),
  vectorize_all = FALSE
)

starting_statements <- ""


specify_input <- "

double *px = REAL(x);
double *py = REAL(y);

"

specify_output <- "

SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
double *pout;
pout = REAL(out);


"

docode <- "tempout = px[flatind_x] + py[flatind_y]"

txt_plus <- stringi::stri_replace_all(
  txt_plus,
  fixed = c("<starting_statements>", "<specify_input>", "<specify_output>", "<docode>", "<closing_statements>"),
  replacement = c(starting_statements, specify_input, specify_output, docode, "UNPROTECT(1);"),
  vectorize_all = FALSE
)

txt_plus <- stringi::stri_c(header, txt_plus, collapse = "\n\n")

cat(txt_plus)


Rcpp::sourceCpp(code = txt_plus)



################################################################################
# perform tests ====
#

x.dim <- c(50:48)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1.1, c(1,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- cumprod(dim(x))
dimcumprod_y <- cumprod(dim(y))
out.dim <- pmax(dim(x), dim(y))
out.len <- prod(out.dim)
dimcumprod_out <- cumprod(out.dim)
inds_x <- lapply(out.dim, \(n)1:n)
inds_y <- lapply(out.dim, \(n)rep(1L, n))

expected <- as.vector(x + drop(y))

out1 <- .rcpp_plus_double_3d(
  x, y, inds_x[[1]], inds_x[[2]], inds_x[[3]], inds_y[[1]], inds_y[[2]], inds_y[[3]],
  dimcumprod_x, dimcumprod_y
)
out2 <- .rcpp_plus_double_general(
  x, y, inds_x, inds_y, dim(x), dim(y), out.len
)

tinytest::expect_equal(expected, out1)
tinytest::expect_equal(expected, out2)
tinytest::expect_equal(expected, outer(x, y, "+") |> as.vector())

px <- as.integer(x)
py <- as.integer(y)

foo <- bench::mark(
  base = as.vector(x + drop(y)),
  outer = outer(x, y, "+") |> as.vector(),
  bc_d = .rcpp_plus_double_3d(
    x, y, inds_x[[1]], inds_x[[2]], inds_x[[3]], inds_y[[1]], inds_y[[2]], inds_y[[3]],
    dimcumprod_x, dimcumprod_y
  ),
  # bc_general = .rcpp_plus_double_double_double_general(
  #   x, y, inds_x, inds_y, dim(x), dim(y), out.len
  # ),
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)



# bigger benchmark

x.dim <- c(50:48)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1.1:50.1, c(50,1,1))
# x + y # gives error; hence need broadcasting
dimcumprod_x <- cumprod(dim(x))
dimcumprod_y <- cumprod(dim(y))
out.dim <- pmax(dim(x), dim(y))
out.len <- prod(out.dim)
dimcumprod_out <- cumprod(out.dim)
inds_x <- lapply(out.dim, \(n) 1:n)
inds_y <- list(1:50, rep(1L, 49), rep(1L, 48))


out1 <- .rcpp_plus_double_3d(
  x, y, inds_x[[1]], inds_x[[2]], inds_x[[3]], inds_y[[1]], inds_y[[2]], inds_y[[3]],
  dimcumprod_x, dimcumprod_y
)
# out2 <- .rcpp_plus_double_double_double_general(
#   x, y, inds_x, inds_y, dim(x), dim(y), out.len
# )
# tinytest::expect_equal(
#   out1, out2
# )


foo <- bench::mark(
  base = as.vector(x + drop(y)),
  outer = outer(x, y, "+") |> as.vector(),
  bc_d = .rcpp_plus_double_3d(
    x, y, inds_x[[1]], inds_x[[2]], inds_x[[3]], inds_y[[1]], inds_y[[2]], inds_y[[3]],
    dimcumprod_x, dimcumprod_y
  ),
  # bc_general = .rcpp_plus_double_double_double_general(
  #   x, y, inds_x, inds_y, dim(x), dim(y), out.len
  # ),
  check = FALSE, # because base and outer wouldn't work correctly here
  min_iterations = 500
)
summary(foo)
ggplot2::autoplot(foo)


