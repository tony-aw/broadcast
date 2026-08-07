#include <Rcpp/Lightest>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_max_ndim)]]
int rcpp_max_ndim(SEXP xndim0, SEXP yndim0) {
  
  if (TYPEOF(xndim0) != INTSXP || Rf_length(xndim0) != 1 ||
      TYPEOF(yndim0) != INTSXP || Rf_length(yndim0) != 1) {
    stop("Bad inputs given in `rcpp_max_ndim()`");
  }

  const int xndim = INTEGER(xndim0)[0];
  const int yndim = INTEGER(yndim0)[0];
  
  int ndim = xndim > yndim ? xndim : yndim;
  
  if(ndim > 16) {
    stop("no more than 16 dimensions allowed");
  }
  else if(ndim > 4) {
    ndim = 16;
  }
  else if(ndim <= 4) {
    ndim = 4;
  }
  
  return ndim;

}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_virt_alloc_dim)]]
IntegerVector rcpp_virt_alloc_dim(
  SEXP x, int ndim
) {
  
  if(ndim < 0) {
    stop("bad input given in `rcpp_virt_alloc_dim()`: `ndim` cannot be negative");
  }
  const int nx = Rf_length(x);
  if(nx > 0 && TYPEOF(x) != INTSXP) {
    stop("bad input given in `rcpp_virt_alloc_dim()`:  `x` is not INTEGER or zero-length");
  }
  
  IntegerVector out(ndim);
  int *pout = INTEGER(out);
  
  if(nx == 0) {
    for(int i = 0; i < ndim; ++i) {
      pout[i] = 1;
    }
    return out;
  }

  int *px = INTEGER(x);
  
  if(nx == ndim) {
    for(int i = 0; i < ndim; ++i) {
      pout[i] = px[i];
    }
    return out;
  }
  
  if(nx < ndim) {
    for(int i = 0; i < nx; ++i) {
      pout[i] = px[i];
    }
    for(int i = nx; i < ndim; ++i) {
      pout[i] = 1;
    }
    return out;
  }
  
  stop("bad input given in `rcpp_virt_alloc_dim()`");
}


 //' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_virt_conformalize)]]
  void rcpp_virt_conformalize(
    SEXP x_dim, SEXP y_dim, SEXP x_ndim, SEXP y_ndim, R_xlen_t xlen, R_xlen_t ylen
  ) {
    
    if (TYPEOF(x_ndim) != INTSXP || Rf_length(x_ndim) != 1 ||
        TYPEOF(y_ndim) != INTSXP || Rf_length(y_ndim) != 1) {
      stop("Bad inputs given in `rcpp_virt_conformalize()`");
    }
    
    if(TYPEOF(x_dim) != INTSXP || TYPEOF(y_dim) != INTSXP) {
      stop("Bad inputs given in `rcpp_virt_conformalize()`");
    }
    
    // NORMALIZE:
    double intmax = pow(2, 31) - 1;
    
    int *px_ndim = INTEGER(x_ndim);
    int *py_ndim = INTEGER(y_ndim);
    int *pxdim = INTEGER(x_dim);
    int *pydim = INTEGER(y_dim);
  
    if(px_ndim[0] > 0 || py_ndim[0] > 0) {
      
      if(px_ndim[0] == 0) {
        if(xlen > intmax) {
          stop("broadcasting will exceed maximum size");
        }
        px_ndim[0] = py_ndim[0];
        pxdim[0] = xlen;
      }
      if(py_ndim[0] == 0) {
        if(ylen > intmax) {
          stop("broadcasting will exceed maximum size");
        }
        py_ndim[0] = px_ndim[0];
        pydim[0] = ylen;
      }
      
      if(px_ndim[0] > py_ndim[0]) {
        py_ndim[0] = px_ndim[0];
      }
      else if(py_ndim[0] > px_ndim[0]) {
        px_ndim[0] = py_ndim[0];
      }
      
    }
    
    
    // CHECK CONFORMABLE:
    const int x_ndim_ro = INTEGER_RO(x_ndim)[0];
    const int y_ndim_ro = INTEGER_RO(y_ndim)[0];
    if(x_ndim_ro == 0 || y_ndim_ro == 0) { // if either dimension is NULL
      // Note: don't have to check if one of inputs has dimensions and the other has no dimensions,
      // because if either is an array,
      // both x and y will be normalized to have the same number of dimensions
      
      if(xlen != ylen) {
        if(xlen != 1 && ylen != 1) {
          stop("arrays not conformable for broadcasting");
        }
      }
    } // end if either dimension is NULL
    else {
      for(int i = 0; i < x_ndim_ro; ++i) {
        if(pxdim[i] != 1 && pydim[i] != 1 && pxdim[i] != pydim[i]) {
          stop("arrays not conformable for broadcasting");
        }
      }
    }
  
  }
  


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_virt_make_outdim_orig)]]
RObject rcpp_virt_make_outdim_orig(
  RObject x, RObject y, SEXP x_dim, SEXP y_dim, int x_ndim_ro, int y_ndim_ro
) {
  if(x_ndim_ro == 0 && y_ndim_ro == 0) {
    return R_NilValue;
  }
  else if(x_ndim_ro != 0 && y_ndim_ro != 0) {
    int n = y_ndim_ro > x_ndim_ro ? y_ndim_ro : x_ndim_ro;
    const int *px = INTEGER_RO(x_dim);
    const int *py = INTEGER_RO(y_dim);
    IntegerVector out(n);
    int *pout = INTEGER(out);
    R_xlen_t out_len = 1;
    
    for(int i = 0; i < n; ++i) {
      pout[i] = py[i] > px[i] ? py[i] : px[i];
      out_len *= pout[i];
    }
    
    const double maxlong = pow(2, 52) - 1;
    if(out_len > maxlong) {
      stop("broadcasting will exceed maximum size");
    }
    
    return(out);
  }
  else if(x_ndim_ro != 0) {
    RObject out = x.attr("dim");
    return out;
  }
  else {
    RObject out = y.attr("dim");
    return out;
  }
}

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_virt_make_outlen)]]
R_xlen_t rcpp_virt_make_outlen(
  SEXP out_dim, R_xlen_t xlen, R_xlen_t ylen
) {

  if(Rf_isNull(out_dim)) {
    return xlen > ylen ? xlen : ylen;
  }
  
  if(TYPEOF(out_dim) != INTSXP) {
    stop("bad input given in `rcpp_virt_make_outlen()`");
  }
  R_xlen_t outprod = 1;
  int n = Rf_length(out_dim);
  const int *pout_dim = INTEGER_RO(out_dim);
  for(int i = 0; i < n; ++i) {
    outprod *= (double)pout_dim[i];
  }
  
  return outprod;
  
  
}


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_virt_part1_test)]]
List rcpp_virt_part1_test(
  RObject x, RObject y, SEXP x_ndim, SEXP y_ndim
) {
  
  // part 1:
  R_xlen_t xlen = Rf_xlength(x);
  R_xlen_t ylen = Rf_xlength(y);
  
  int ndim = rcpp_max_ndim(x_ndim, y_ndim);
  
  IntegerVector x_dim = rcpp_virt_alloc_dim(x.attr("dim"), ndim);
  IntegerVector y_dim = rcpp_virt_alloc_dim(y.attr("dim"), ndim);
  
  rcpp_virt_conformalize(x_dim, y_dim, x_ndim, y_ndim, xlen, ylen);
  
  
  RObject outdim_orig = rcpp_virt_make_outdim_orig(
    x, y, x_dim, y_dim, INTEGER_RO(x_ndim)[0], INTEGER_RO(y_ndim)[0]
  );
  R_xlen_t outlen = rcpp_virt_make_outlen(outdim_orig, xlen, ylen);
  
  
  List out(6);
  out[0] = x_dim;
  out[1] = y_dim;
  out[2] = outdim_orig;
  out[3] = outlen;
  out[4] = x_ndim;
  out[5] = y_ndim;
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_virt_drop_dims)]]
void rcpp_virt_drop_dims(
  SEXP x_dim, SEXP y_dim, SEXP x_ndim, SEXP y_ndim, R_xlen_t xlen, R_xlen_t ylen
) {
  int *px_ndim = INTEGER(x_ndim);
  int *py_ndim = INTEGER(y_ndim);
  int *px = INTEGER(x_dim);
  int *py = INTEGER(y_dim);
  
  // nullify dims in obvious cases:
  if(xlen == 1 || ylen == 1 || (px_ndim[0] <= 1 && py_ndim[0] <= 1)) {
    px_ndim[0] = 0;
    py_ndim[0] = 0;
    
    int len = Rf_length(x_dim);
    for(int i = 0; i < len; ++i) {
      px[i] = 1;
      py[i] = 1;
    }
    
    return;
  }
  
  
  // drop common 1s:
  const int ndim = py_ndim[0] > px_ndim[0] ? py_ndim[0] : px_ndim[0];
  if(ndim > 1) {
    
    std::vector<int> bufx(ndim);
    std::vector<int> bufy(ndim);
    int count = 0;
    for(int i = 0; i < ndim; ++i) {
      if(px[i] != 1 || py[i] != 1) {
        bufx[count] = px[i];
        bufy[count] = py[i];
        count++;
      }
    }
  
    if(count == 0) {
      px_ndim[0] = 0;
      py_ndim[0] = 0;
      for(int i = 0; i < ndim; ++i) {
        px[i] = 1;
        py[i] = 1;
      }
    }
    else if(count) {
      for(int i = 0; i < count; ++i) {
        px[i] = bufx[i];
        py[i] = bufy[i];
      }
      if(count < ndim) {
        for(int i = count; i < ndim; ++i) {
          px[i] = 1;
          py[i] = 1;
        }
      }
      px_ndim[0] = count;
      py_ndim[0] = count;
    }
  }
    
}




// 2 ADJACENT dimensions i and i+1 of arrays x and y can be merged if,
// and only if, ALL of the following is TRUE:
//  -> dim(x)[i] and dim(x)[i + 1] are not auto-orthogonal AND dim(y)[i] and dim(y)[i + 1] are not auto-orthogonal
//  -> (dim(x)[i] * dim(x)[i + 1]) < (2^31-1).
//  -> (dim(y)[i] * dim(y)[i + 1]) < (2^31-1).
// i.e. if x.dim[1:2] = c(1, 1) and y.dim[1:2] = c(2, 3),
// x.dim[1:2] can be merged to become 1 and y.dim[1:2] to become 6 (= prod(c(2, 3))).
// But if x.dim[1:3] = c(1, 9, 1) and y.dim = c(8, 1, 8),
// x.dim[1:3] is auto-orthogonal, and so is y.dim[1:3], and thus they CANNOT be merged.
// Merging prevents unnecessary broadcasting,
// which in turn makes the actual broadcasting more efficient.

// Note that the following is probably not the most efficient code.
// The code was written primarily to be readible.
// The power of C++ is used to make it less slow than 'R'.




//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims_get_endrange)]]
 int rcpp_mergedims_get_endrange(SEXP x, SEXP y, int pos, double intmax) {
   
   int *px = INTEGER(x);
   int *py = INTEGER(y);
   int n = Rf_length(x);
   
   bool merge_x, merge_y, drop_next;
   double prod_x = (double)px[pos];
   double prod_y = (double)py[pos];
   
   
   if(pos == (n - 1)) { // if `pos` is last position, exit and return pos;
     return pos;
   }
   
   // else, start at next position:
   int i;
   for(i = (pos + 1); i < n; ++i) {
     merge_x = (px[pos] == 1) == (px[i] == 1);
     merge_y = (py[pos] == 1) == (py[i] == 1);
     drop_next = (px[i] == 1) && (py[i] == 1);
     if((merge_x && merge_y) || drop_next) {
       prod_x *= (double)px[i];
       prod_y *= (double)py[i];
       if((prod_x >= intmax) || (prod_y >= intmax)) {
         return (i - 1);
       }
     }
     else {
       return (i - 1);
     }
   }
   
   return i -1;
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims_get_prods)]]
 Rcomplex rcpp_mergedims_get_prods(SEXP x, SEXP y, int start, int end) {
   
   int *px = INTEGER(x);
   int *py = INTEGER(y);
   
   double prod_x = (double)px[start];
   double prod_y = (double)py[start];
   Rcomplex out;
   
   
   // if start = i and end = i, this if() statement is run:
   if(end == start) {
     out.r = prod_x;
     out.i = prod_y;
     return out;
   }
   
   // start+1 because we don't want to multiply x[i] with itself;
   // i <= end instead of i < end, because (unlike `n`) `end` is always smaller than length(x);
   for(int i = (start + 1); i <= end; ++i) {
     prod_x *= px[i];
     prod_y *= py[i];
   }
   
   out.r = prod_x;
   out.i = prod_y;
   return out;
   
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_mergedims)]]
 SEXP rcpp_mergedims(SEXP x, SEXP y) {
   
   int n = Rf_length(x);
   int *bufx = (int *) R_alloc(n, sizeof(int));
   int *bufy = (int *) R_alloc(n, sizeof(int));
   SEXP outx;
   SEXP outy;
   
   int start = 0;
   int end = 0;
   Rcomplex prods;
   double intmax = pow(2, 31) - 1;
   
   int i;
   
   for(i = 0; i < n; ++i) {
     end = rcpp_mergedims_get_endrange(x, y, start, intmax);
     prods = rcpp_mergedims_get_prods(x, y, start, end);
     bufx[i] = (int) (prods.r);
     bufy[i] = (int) (prods.i);
     start = end + 1;
     
     if(end >= (n - 1)) {
       // n - 1 because cpp starts counting at 0 (obviously);
       break;
     }
     
   }
   
   int len = i + 1; // again, cpp starts counting at zero, but we want the length
   
   PROTECT(outx = Rf_allocVector(INTSXP, len));
   PROTECT(outy = Rf_allocVector(INTSXP, len));
   if(len) {
     memcpy(INTEGER(outx), bufx, sizeof(int) * len);
     memcpy(INTEGER(outy), bufy, sizeof(int) * len);
     
   }
   
   SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
   SET_VECTOR_ELT(out, 0, outx);
   SET_VECTOR_ELT(out, 1, outy);
   
   UNPROTECT(3);
   
   return out;
   
 }

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_mergedims_set)]]
void rcpp_mergedims_set(SEXP x_dim, SEXP y_dim, SEXP x_ndim, SEXP y_ndim) {
  
  if (TYPEOF(x_ndim) != INTSXP || Rf_length(x_ndim) != 1 ||
      TYPEOF(y_ndim) != INTSXP || Rf_length(y_ndim) != 1) {
    stop("Bad inputs given in `rcpp_mergedims_set()`");
  }
  
  if(TYPEOF(x_dim) != INTSXP || TYPEOF(y_dim) != INTSXP) {
    stop("Bad inputs given in `rcpp_mergedims_set()`");
  }
  
  SEXP mergedims = PROTECT(rcpp_mergedims(x_dim, y_dim));
  
  // get properties:
  SEXP mx = VECTOR_ELT(mergedims, 0);
  SEXP my = VECTOR_ELT(mergedims, 1);
  
  int *pmx = INTEGER(mx);
  int *pmy = INTEGER(my);
  
  int *px = INTEGER(x_dim);
  int *py = INTEGER(y_dim);
  int *px_ndim = INTEGER(x_ndim);
  int *py_ndim = INTEGER(y_ndim);
  
  
  // reset dims:
  for(int i = 0; i < Rf_length(x_dim); ++i) {
    px[i] = 1;
    py[i] = 1;
  }
  
  // refill in dims:
  const int n_m = Rf_length(mx);
  px_ndim[0] = Rf_length(mx);
  py_ndim[0] = Rf_length(my);
  
  for(int i = 0; i < n_m; ++i) {
    px[i] = pmx[i];
    py[i] = pmy[i];
  }
  
  UNPROTECT(1);
   
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_virt_dimmode)]]
int rcpp_virt_dimmode(
  SEXP xdim, SEXP ydim, const R_xlen_t xlen, const R_xlen_t ylen, const int ndim
) {
  
  if(xlen == 1L || ylen == 1L) { // x and/or y are/is scalar(s)
    return(1);
  }
  else if(ndim <= 1) { // x and y are vectors or 1d arrays
    return(1);
  }
  
  const int *pxdim = INTEGER_RO(xdim);
  const int *pydim = INTEGER_RO(ydim);
  int dims_all_equal = 1;
  for(int i = 0; i < ndim; ++i) {
    if(pxdim[i] != pydim[i]) {
      dims_all_equal = 0;
      break;
    }
  }
  if(dims_all_equal) { // x and y are arrays of equal dimensions
    return(1);
  }
  
  
  
  // Use OrthoVector mode ======================================================
  
  if(ndim == 2) {
    int check1 = pxdim[0] != pydim[0] && pxdim[1] != pydim[1];
    int check2 = pxdim[0] != pxdim[1] && pydim[0] != pydim[1];
    int check3 = pxdim[0] == 1 || pxdim[1] == 1 || pydim[0] == 1 || pydim[1] == 1;
    if(check1 && check2 && check3) {
      return(2);
    }
  }
  
  
  
  // Use Big2Vector mode =======================================================
  
  // IF merging of dimensions is successful, a big array by vector mode can only come in 2 forms:
  //  - A matrix with a vector at end a la c(1, n)/c(n, 1);
  //  - a 3d array with a sandwiched vector a la c(1, n, 1).

  if(ndim == 2) {
    // If ndim == 2, and it's not of mode vector or orhtovector, it can only be of mode big2vector
    return(3);
  }
  
  if( ndim == 3 ) {
    // for 3 dimensions, the big2vector MACRO I made only supports sandwiched vectors, so this must be checked carefully
    
    const int middlex = (pxdim[0] == 1) && (pxdim[1] > 1) && (pxdim[2] == 1);
    const int middley = (pydim[0] == 1) && (pydim[1] > 1) && (pydim[2] == 1);
    const int bigx = (pxdim[0] >= pydim[0]) && (pxdim[1] >= pydim[1]) && (pxdim[2] >= pydim[2]);
    const int bigy = (pydim[0] >= pxdim[0]) && (pydim[1] >= pxdim[1]) && (pydim[2] >= pxdim[2]);
    
    if(middlex && bigy) {
      return(3);
    }
    if(middley && bigx) {
      return(3);
    }
    
    const int sandwichx = (pxdim[0] > 1) && (pxdim[1] == 1) && (pxdim[2] > 1);
    const int sandwichy = (pydim[0] > 1) && (pydim[1] == 1) && (pydim[2] > 1);
    
    if(middlex && sandwichy) {
      return(4);
    }
    if(middley && sandwichx) {
      return(4);
    }
    
  }
  
  
  // Use General Mode ==========================================================
  return(5);
  
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_virt_make_outdim_simp)]]
RObject rcpp_virt_make_outdim_simp(
  SEXP x_dim, SEXP y_dim, const int n
) {

  const int *px = INTEGER_RO(x_dim);
  const int *py = INTEGER_RO(y_dim);
  
  if(n == 0) {
    return R_NilValue;
  }
  
  if(Rf_length(x_dim) != Rf_length(y_dim)) {
    stop("bad input given in `rcpp_virt_make_outdim_simp`");
  }
  
  int nout;
  
  if(n <= 4) {
    nout = 4;
  }
  else {
    nout = 16;
  }
  
  IntegerVector out(nout);
  int *pout = INTEGER(out);
  int i;
  for(i = 0; i < n; ++i) {
    pout[i] = py[i] > px[i] ? py[i] : px[i];
  }
  if(i < nout) {
    for(; i < nout; ++i) {
      pout[i] = 1;
    }
  }
  
  return out;
  
}
  





//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_virt_binary_prep)]]
List rcpp_virt_binary_prep(
  RObject x, RObject y, SEXP x_ndim, SEXP y_ndim
) {
  
  // part 1:
  R_xlen_t xlen = Rf_xlength(x);
  R_xlen_t ylen = Rf_xlength(y);
  
  int ndim = rcpp_max_ndim(x_ndim, y_ndim);
  
  IntegerVector x_dim = rcpp_virt_alloc_dim(x.attr("dim"), ndim);
  IntegerVector y_dim = rcpp_virt_alloc_dim(y.attr("dim"), ndim);
  
  rcpp_virt_conformalize(x_dim, y_dim, x_ndim, y_ndim, xlen, ylen);
  
  RObject outdim_orig = rcpp_virt_make_outdim_orig(
    x, y, x_dim, y_dim, INTEGER_RO(x_ndim)[0], INTEGER_RO(y_ndim)[0]
  );
  R_xlen_t outlen = rcpp_virt_make_outlen(outdim_orig, xlen, ylen);
  
  
  
  // part 2:
  rcpp_virt_drop_dims(x_dim, y_dim, x_ndim, y_ndim, xlen, ylen);
  
  if(INTEGER_RO(x_ndim)[0] > 2 && INTEGER_RO(y_ndim)[0] > 2) {
    rcpp_mergedims_set(x_dim, y_dim, x_ndim, y_ndim);
  }
  
  int dimmode = rcpp_virt_dimmode(x_dim, y_dim, xlen, ylen, INTEGER(x_ndim)[0]);
  
  if(dimmode == 3 && INTEGER_RO(x_ndim)[0] == 2) {
    // starting params:
    int *px = INTEGER(x_dim);
    int *py = INTEGER(y_dim);
    
    if(px[0] > 1 && py[0] > 1 && (px[1] == 1 || py[1] == 1)) {
        // vector dims = c(n, 1)
        px[2] = px[1];
        px[1] = px[0];
        px[0] = 1;
        py[2] = py[1];
        py[1] = py[0];
        py[0] = 1;
        INTEGER(x_ndim)[0] = 3;
        INTEGER(y_ndim)[0] = 3;
        
      }
      else if((px[0] == 1 || py[0] == 1) && px[1] > 1 && py[1] > 1) {
        // vector dims = c(1, n)
        px[2] = 1;
        py[2] = 1;
        INTEGER(x_ndim)[0] = 3;
        INTEGER(y_ndim)[0] = 3;
      }
    
  }
  
  
  // part 3:
  RObject outdim_simp = rcpp_virt_make_outdim_simp(x_dim, y_dim, INTEGER_RO(x_ndim)[0]);
  
  List out(6);
  out[0] = x_dim;
  out[1] = y_dim;
  out[2] = outdim_orig;
  out[3] = outdim_simp;
  out[4] = outlen;
  out[5] = dimmode;
  
  return out;
  
}
