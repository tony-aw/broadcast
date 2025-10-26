
#' @rdname linear_algebra_stats
#' @export
cinv <- function(x) {
  return(chol2inv(chol(x)))
}

#' @rdname linear_algebra_stats
#' @export
sd_lc <- function(
    X, vc, bad_rp = NaN
){
  
  # check input:
  check_X <- is.matrix(X) && (is.numeric(X) || is.logical(X)) && length(X) >= 1L
  if(!check_X) {
    stop("`X` must be a numeric or logical matrix")
  }
  check_vc <- is.matrix(vc) && is.double(vc) && length(vc) >= 1L
  if(!check_vc) {
    stop("`vc` must be a variance-covariance matrix")
  }
  if(length(bad_rp) != 1L || !is.numeric(bad_rp)) {
    stop("`bad_rp` must be a numeric scalar")
  }
  bad_rp <- as.double(bad_rp)
  
  # check lengths & dimensions:
  check_dims <- all(ncol(X) == dim(vc))
  if(!check_dims) {
    stop("`X` and `vc` do not have correctly corresponding dimensions!")
  }
  
  return(.C_sd_lc(X, vc, nrow(X), ncol(X), bad_rp))
  
  
}


