#' Details on the Binding Implementations in 'broadcast'
#'
#' @description
#' This help page gives additional details on the binding implementations in the 'broadcast' package. \cr
#' \cr
#' 
#' 
#' @section Empty inputs:
#' If argument `input` has length `0`,
#' or it contains exclusively objects where one or more dimensions are `0`,
#' an error is returned. \cr
#' \cr
#' If `input` has length `1`, these functions simply return `input[[1L]]`. \cr
#' \cr
#' 
#' 
#' @section Differences with `abind()`, `rbind()`/`cbind()`:
#' 
#' The API of `bind_array()` is inspired by the fantastic
#' \code{abind::abind} function
#' by Tony Plare & Richard Heiberger (2016). \cr
#' But `bind_array()` differs considerably from \code{abind::abind}
#' in the following ways:
#'  
#'  - `bind_array()` differs from \code{abind::abind}
#'  in that it can handle recursive arrays properly \cr
#'  (the \code{abind::abind} function would unlist everything to atomic arrays,
#'  ruining the structure).
#'  - `bind_array()` allows for broadcasting,
#'  while \code{abind::abind} does not support broadcasting.
#'  - `bind_array()` is generally faster than \code{abind::abind},
#'  as `bind_array()` relies heavily on 'C' and 'C++' code.
#'  - unlike \code{abind::abind},
#'  `bind_array()` only binds (atomic/recursive) arrays and matrices. \cr
#'  `bind_array()`does not attempt to convert things to arrays when they are not arrays,
#'  but will give an error instead. \cr
#'  This saves computation time and prevents unexpected results.
#'  - `bind_array()` has more streamlined naming options,
#'  compared to \code{abind::abind}. \cr \cr
#'  
#'  
#' `bind_mat()` is a modified version of base R's `rbind()`/`cbind()` functions. \cr
#' `bind_mat()` differs from `rbind()`/`cbind()` in the following ways:
#' 
#'  - it has more streamlined naming options/
#'  - `bind_mat()` gives an error when fractional recycling is attempted
#'  (like binding  `1:3` with `1:10`). \cr \cr
#' 
#' 

#' @rdname aaa01_broadcast_bind
#' @name aaa01_broadcast_bind
#' @aliases broadcast_bind
NULL

