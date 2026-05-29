
#' @keywords internal
#' @noRd
.acast_set_dimnames <- function(out, x, margin, grp_lvls) {
  
  
  out.dimnames <- rep(list(NULL), ndim(out))
  out.dimnames[seq_len(ndim(x))] <- dimnames(x)
  out.dimnames[margin] <- list(NULL)
  out.dimnames[ndim(x) + 1L] <- list(grp_lvls) # safe, because I used droplevels()
  
  .set_dimnames(out, out.dimnames)
}
