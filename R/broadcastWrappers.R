

#' @keywords Internal
#' @noRd
.C_any_nonNULL <- function(x) {
  .Call("C_any_nonNULL", x = x)
}


#' @keywords Internal
#' @noRd
.C_arraysize_overflow <- function(xdim, xlen) {
  .Call("C_arraysize_overflow", xdim, xlen)
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
.C_dims_is_vector <- function(xdim) {
  .Call("C_dims_is_vector", xdim)
}

#' @keywords Internal
#' @noRd
.C_recycle_seq_dim <- function(x, y) {
  .Call("C_recycle_seq_dim", as.integer(x), as.integer(y))
}



#' @keywords Internal
#' @noRd
.C_unlisthelper_maxlen <- function(x) {
  .Call("C_unlisthelper_maxlen", x)
}


# linear algebra =====

#' @keywords Internal
#' @noRd
.C_sd_lc <- function(w, vc, nobs, nvars, bad_rp) {
  .Call("C_sd_lc", w, diag(vc), vc, as.integer(nobs), as.integer(nvars), as.double(bad_rp))
}



# bindhelpers ====

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
.C_bind_which_comdims <- function(out_dim, start, obj_dim) {
  out_dim <- as.integer(out_dim)
  start <- as.integer(start)
  obj_dim <- as.integer(obj_dim)
  .Call("C_bind_which_comdims", out_dim, start, obj_dim)
}

