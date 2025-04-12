
#' @keywords Internal
#' @noRd
.C_check_conf_dim <- function(x, y) {
  .Call("C_check_conf_dim", x = as.integer(x), y = as.integer(y))
}

#' @keywords Internal
#' @noRd
.C_pmax <- function(x, y) {
  .Call("C_pmax", x = as.integer(x), y = as.integer(y))
}


#' @keywords Internal
#' @noRd
.C_dims_all_orthogonal <- function(xdim, ydim) {
  .Call("C_dims_all_orthogonal", xdim = as.integer(xdim), ydim = as.integer(ydim))
}


#' @keywords Internal
#' @noRd
.C_any_nonNULL <- function(x) {
  .Call("C_any_nonNULL", x = x)
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
.C_seq_Clen <- function(start, len) {
  if(typeof(start) != typeof(len)) {
    start <- as.numeric(start)
    len <- as.numeric(len)
  }
  .Call("C_seq_Clen", start = start, len = len)
}

#' @keywords Internal
#' @noRd
.C_bind_which_comnames <- function(out_dim, start, obj_dim) {
  out_dim <- as.integer(out_dim)
  start <- as.integer(start)
  obj_dim <- as.integer(obj_dim)
  .Call("C_bind_which_comnames", out_dim, start, obj_dim)
}

