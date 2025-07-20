#' Simple Linear Algebra Functions for Statistics
#'
#' @description
#' 'broadcast' provides some simple Linear Algebra Functions for Statistics: \cr
#' `cinv()`; \cr
#' `sd_lc()`. \cr
#' \cr
#' \cr
#' 
#' @param x a real symmetric positive-definite square matrix.
#' @param X a numeric (or logical) matrix of multipliers/constants
#' @param vc the variance-covariance matrix for the (correlated) random variables.
#' @param bad_rp if `vc` is not a Positive (semi-) Definite matrix,
#' give here the value to replace bad standard deviations with. \cr \cr
#' 
#' @details
#' \bold{cinv()} \cr
#' `cinv()`
#' computes the Choleski inverse
#' of a real symmetric positive-definite square matrix. \cr
#' \cr
#' \bold{sd_lc()} \cr
#' Given the linear combination `X %*% b`, where:
#' 
#'  - `X` is a matrix of multipliers/constants;
#'  - `b` is a vector of (correlated) random variables;
#'  - `vc` is the symmetric variance-covariance matrix for `b`;
#' 
#' `sd_lc(X, vc)`
#' computes the standard deviations for the linear combination `X %*% b`,
#' without making needless copies. \cr
#' `sd_lc(X, vc)` will use \bold{much} less memory than a base 'R' approach. \cr
#' `sd_lc(X, vc)` may possibly, but not necessarily, be faster than a base 'R' approach
#' (depending on the Linear Algebra Library used for base 'R'). \cr
#' \cr
#' \cr
#' 
#' 
#'
#' @returns
#' For `cinv()`: \cr
#' A matrix. \cr
#' \cr
#' For `sd_lc()`: \cr
#' A vector of standard deviations.
#' \cr
#' \cr
#'
#'
#' @seealso \link[base]{chol}, \link[base]{chol2inv}
#' @references John A. Rice (2007), \emph{Mathematical Statistics and Data Analysis} (6th Edition) 
#'
#' @example inst/examples/linear_algebra_stats.R
#' 

#' @name linear_algebra_stats
NULL
