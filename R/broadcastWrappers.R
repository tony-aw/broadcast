
#' @keywords Internal
#' @noRd
.C_check_conf_dim <- function(xdim, ydim, xlen, ylen) {
  .Call("C_check_conf_dim", xdim, ydim, xlen, ylen)
}

#' @keywords Internal
#' @noRd
.C_pmax <- function(x, y) {
  .Call("C_pmax", x = as.integer(x), y = as.integer(y))
}



#' @keywords Internal
#' @noRd
.C_any_nonNULL <- function(x) {
  .Call("C_any_nonNULL", x = x)
}


#' @keywords Internal
#' @noRd
.C_any_nonarray <- function(x) {
  .Call("C_any_nonarray", x = x)
}


#' @keywords Internal
#' @noRd
.C_arraysize_overflow <- function(xdim, xlen) {
  .Call("C_arraysize_overflow", xdim, xlen)
}


#' @keywords Internal
#' @noRd
.C_lst_ndims <- function(x) {
  .Call("C_lst_ndims", x = x)
}

#' @keywords Internal
#' @noRd
.C_make_by <- function(x) {
  .Call("C_make_by", target_dim = as.integer(x))
}


#' @keywords Internal
#' @noRd
.C_make_dcp <- function(x) {
  .Call("C_make_dcp", target_dim = as.integer(x))
}



#' @keywords Internal
#' @noRd
.C_seq_Clen <- function(start, len) {
  if(typeof(start) != typeof(len)) {
    start <- as.numeric(start)
    len <- as.numeric(len)
  }
  .Call("C_seq_Clen", start = start, len = len)
}

#' @keywords Internal
#' @noRd
.C_bind_which_comdims <- function(out_dim, start, obj_dim) {
  out_dim <- as.integer(out_dim)
  start <- as.integer(start)
  obj_dim <- as.integer(obj_dim)
  .Call("C_bind_which_comdims", out_dim, start, obj_dim)
}


#' @keywords Internal
#' @noRd
.C_dropdims_count <- function(xdim, ydim) {
  .Call("C_dropdims_count", xdim, ydim)
}

#' @keywords Internal
#' @noRd
.C_dropdims_which <- function(xdim, ydim, size) {
  .Call("C_dropdims_which", xdim, ydim, size)
}


#' @keywords Internal
#' @noRd
.C_dims_allge <- function(xdim, ydim) {
  .Call("C_dims_allge", xdim, ydim)
}

#' @keywords Internal
#' @noRd
.C_binames_consider_dim <- function(outdim, xdim, xdimnames) {
  .Call("C_binames_consider_dim", outdim, xdim, xdimnames)
}

#' @keywords Internal
#' @noRd
.C_recycle_seq_dim <- function(x, y) {
  .Call("C_recycle_seq_dim", as.integer(x), as.integer(y))
}


#' @keywords Internal
#' @noRd
.C_sd_lc <- function(w, vc, nobs, nvars, bad_rp) {
  .Call("C_sd_lc", w, diag(vc), vc, as.integer(nobs), as.integer(nvars), as.double(bad_rp))
}


#' @keywords Internal
#' @noRd
.C_bindhelper_vdims <- function(x) {
  .Call("C_bindhelper_vdims", x)
}

#' @keywords Internal
#' @noRd
.C_bindhelper_sum_along <- function(lst_dims, along) {
  .Call("C_bindhelper_sum_along", lst_dims, as.integer(along))
}

#' @keywords Internal
#' @noRd
.C_bindhelper_max_type <- function(x) {
  .Call("C_bindhelper_max_type", x)
}

#' @keywords Internal
#' @noRd
.C_bindhelper_get_alongdims <- function(lst, along) {
  .Call("C_bindhelper_get_alongdims", lst, as.integer(along))
}


#' @keywords Internal
#' @noRd
.C_bindhelper_need_coerce <- function(lst, mytemplate) {
  .Call("C_bindhelper_need_coerce", lst, mytemplate)
}


#' @keywords Internal
#' @noRd
.C_chunkify_dims <- function(dims, chunks) {
  .Call("C_chunkify_dims", as.integer(dims), as.integer(chunks))
}

#' @keywords Internal
#' @noRd
.C_unlisthelper_maxlen <- function(x) {
  .Call("C_unlisthelper_maxlen", x)
}

#' @keywords Internal
#' @noRd
.C_determine_dimmode <- function(xdim, ydim, xlen, ylen) {
  .Call("C_determine_dimmode", xdim, ydim, xlen, ylen)
}
