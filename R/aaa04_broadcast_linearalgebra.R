#' Simple Linear Algebra Functions for Statistics
#'
#' @description
#' 'broadcast' provides some simple Linear Algebra Functions for Statistics: \cr
#' `cinv()` \cr
#' `sd_lc()` \cr
#' `ecumprob()` \cr
#' \cr
#' \cr
#' 
#' @param x a real symmetric positive-definite square matrix.
#' @param X a numeric (or logical) matrix of multipliers/constants
#' @param vc the variance-covariance matrix for the (correlated) random variables.
#' @param y values to estimate the cumulative probability for.
#' @param sim a matrix (or data.frame) with at least 500 columns of simulated values. \cr
#' If `sim` is given as a dimensionless vector,
#' it will be treated as a matrix with 1 row and `length(sim)` columns,
#' and this will be noted with a message.
#' @param eps a non-negative numeric scaler smaller than `0.1`,
#' giving the cut-off value for probabilities. \cr
#' Probabilities smaller than `eps` will be replaced with `eps`,
#' and probabilities larger than `1 - eps` will be replaced with `1 - eps`. \cr
#' Set `eps = 0` to disable probability trimming.
#' @param bad_rp if `vc` is not a Positive (semi-) Definite matrix,
#' give here the value to replace bad standard deviations with. \cr \cr
#' 
#' @details
#' \bold{`cinv()`} \cr
#' `cinv()`
#' computes the Choleski inverse
#' of a real symmetric positive-definite square matrix. \cr
#' \cr
#' \cr
#' \bold{`sd_lc()`} \cr
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
#' `sd_lc(X, vc)` will \emph{usually} be faster than a base 'R' approach
#' (depending on the Linear Algebra Library used for base 'R'). \cr
#' \cr
#' \cr
#' \bold{`ecumprob()`} \cr
#' The `ecumprod(y, sim)` function takes a matrix (or data.frame) of simulated values `sim`,
#' and for each row `i` (after broadcasting),
#' estimates the cumulative distribution function of `sim[i, ]`,
#' and returns the cumulative probability for `y[i]`. \cr
#' \cr
#' In terms of statistics,
#' it is equivalent to the following operation for each index `i`: \cr
#' `ecdf(sim[i,])(y[i])` \cr
#' However, `ecumprob()` is \bold{much} faster, and supports `NA`s/`NaN`s. \cr
#' \cr
#' In terms of linear algebra,
#' it is equivalent to the following broadcasted operation: \cr
#' `rowMeans(sim <= y)` \cr
#' where `y` and `sim` are \link{broadcaster} arrays. \cr
#' However, `ecumprob()` is \bold{much more} memory-efficient,
#' supports a data.frame for `sim`,
#' and has statistical safety checks. \cr
#' \cr
#' 
#'
#' @returns
#' For `cinv()`: \cr
#' A matrix. \cr
#' \cr
#' For `sd_lc()`: \cr
#' A vector of standard deviations. \cr
#' \cr
#' For `ecumprob()`: \cr
#' A vector of cumulative probabilities. \cr
#' If for any observation `i` (after broadcasting),
#' `y[i]` is NA/NaN or any of `sim[i,]` is NA/NaN,
#' the result for `i` will be `NA`. \cr
#' If zero-length `y` or `sim` is given, a zero-length numeric vector is returned. \cr
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
