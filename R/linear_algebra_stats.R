
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



#' @rdname linear_algebra_stats
#' @export
ecumprob <- function(y, sim, eps = 0.0) {
  
  if(length(y) == 0L || length(sim) == 0L || any(dim(sim) == 0L)) {
    return(numeric(0L))
  }
  
  if(is.null(dim(sim))) {
    message("`sim` is given as a dimensionless vector, and will be treated as a matrix with 1 row and `length(sim)` columns")
  }
  else if(ndim(sim) != 2L) {
    stop("`sim` must be a matrix (or data.frame)")
  }
  
  if(.nsim(sim) < 500L) {
    stop("at least 500 columns of simulated values must be provided")
  }
  
  if(!is.numeric(y) && !is.logical(y)) {
    stop("`y` must be numeric or logical")
  }
  if(!is.null(dim(y))) {
    stop("`y` must be a vector")
  }
  
  if(!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a numeric scalar")
  }
  if(eps < 0 || eps > 0.1) {
    stop("`eps` cannot be smaller than 0 or larger than 0.1")
  }
  
  
  
  if(is.matrix(sim) || is.null(dim(sim))) {
    return(.ecp_mat(y, sim, eps, sys.call()))
  }
  else if(is.data.frame(sim)) {
    return(.ecp_df(y, sim, eps, sys.call()))
  }
  else {
    stop("unsupported form of `sim` given")
  }
  
}



#' @keywords internal
#' @noRd
.ecp_mat <- function(y, sim, eps, abortcall) {
  
  if(!is.numeric(sim) && !is.logical(sim)) {
    stop(simpleError(
      "if `sim` is a matrix, it must be numeric or logical", call = abortcall
    ))
  }
  if(is.numeric(sim) != is.numeric(y)) {
    stop(simpleError(
      "if `sim` is a matrix, `y` and `sim` must be of the same type", call = abortcall
    ))
  }
  
  
  if(is.double(sim) || is.double(y)) {
    if(!is.double(sim)) sim <- as_dbl(sim)
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  if(is.null(dim(sim))) {
    sim.nrow <- 1L
    sim.ncol <- length(sim)
  }
  else {
    sim.nrow <- nrow(sim)
    sim.ncol <- ncol(sim)
  }
  
  return(.rcpp_ecp_mat(y, sim, sim.nrow, sim.ncol, eps))
}



#' @keywords internal
#' @noRd
.ecp_df <- function(y, sim, eps, abortcall) {
  
  sim.nrow <- nrow(sim)
  sim.ncol <- ncol(sim)
  
  if(!is.numeric(sim[[1L]]) && !is.logical(sim[[1L]])) {
    stop("the columns of `sim` must be numeric or logical")
  }
  if(is.double(sim[[1L]]) || is.double(y)) {
    if(!is.double(sim[[1L]])) {
      sim <- lapply(sim, as.double)
    }
    if(!is.double(y)) y <- as_dbl(y)
  }
  
  return(.rcpp_ecp_df(y, sim, sim.nrow, sim.ncol, eps))
}



#' @keywords internal
#' @noRd
.nsim <- function(x) {
  if(is.null(dim(x))) {
    return(length(x))
  }
  else {
    return(ncol(x))
  }
}
