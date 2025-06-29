#' Simple Linear Algebra Functions for Statistics
#'
#' @description
#' 'broadcast' provides some simple Linear Algebra Functions for Statistics: \cr
#' `cinv()`; \cr
#' `sd_gauss_lc()`. \cr
#' \cr
#' \cr
#' 
#' @param x a real symmetric positive-definite square matrix.
#' @param X a numeric matrix of multipliers/constants
#' @param vc the variance-covariance matrix for the (correlated) Gaussian random variables.
#' @param bad_rp if `vc` is not a Positive (semi-) Definite matrix,
#' give here the value to replace bad standard deviations with. \cr \cr
#' 
#' @details
#' \bold{cinv()} \cr
#' `cinv()`
#' computes the Choleski inverse
#' of a real symmetric positive-definite square matrix. \cr
#' \cr
#' \bold{sd_gauss_lc()} \cr
#' Given the linear combination `X %*% b`, where:
#' 
#'  - `X` is a matrix of multipliers/constants;
#'  - `b` is a vector of (correlated) Gaussian random variables;
#'  - `vc` is the variance-covariance matrix for `b`;
#' 
#' `sd_gauss_lc(X, vc)`
#' computes the standard deviations for the linear combination `X %*% b`. \cr
#' Written in 'C' for efficiency. \cr
#' \cr
#' \cr
#' 
#' 
#'
#' @returns
#' For `cinv()`: \cr
#' A matrix. \cr
#' \cr
#' For `sd_gauss_lc()`: \cr
#' A vector of standard deviations.
#' \cr
#' \cr
#'
#' @example inst/examples/linear_algebra_stats.R
#' 


#' @name linear_algebra_stats
NULL


#' @rdname linear_algebra_stats
#' @export
cinv <- function(x) {
  return(chol2inv(chol(x)))
}

#' @rdname linear_algebra_stats
#' @export
sd_gauss_lc <- function(
    X, vc, bad_rp = NaN
){
  
  # check input:
  check_X <- is.matrix(X) && is.numeric(X) && length(X) >= 1L
  if(!check_X) {
    stop("`X` must be a numeric matrix")
  }
  if(is.integer(X)) {
    X <- as_dbl(X)
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
  return(.C_sd_gauss_lc(X, vc, ncol(X), nrow(X), bad_rp))
}


