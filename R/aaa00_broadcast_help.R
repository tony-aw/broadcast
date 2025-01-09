#' broadcast: Subset Methods as Alternatives to the Square Brackets Operators for Programming
#' 
#' @description
#' broadcast: \cr
#' Simple broadcasted binary operations for atomic and recursive arrays in 'R'. \cr
#' 
#' @details
#' The 'broadcast' package provides a set of type-specific functions
#' for broadcasted operations. \cr
#' The functions use an API similar to the \link[base]{outer} and \link[base]{sweep} functions. \cr
#' \cr
#' The following functions are available:
#' 
#'  * \link{bc.num}: Broadcasted operations for numeric (types `int` and `dbl`) arrays.
#'  * `bc.bool`: Broadcasted operations for logical/Boolean arrays.
#'  * `bc.cplx`: Broadcasted operations for `complex` arrays.
#'  * `bc.str`: Broadcasted operations for `character` arrays.
#'  * `bc.list`: Broadcasted operations for recursive arrays.
#'  
#' Each of these functions support 2 types of operations:
#' 
#'  * `regular` operations, which return an array of the same type. \cr
#'  For example: +, -, *, /, etc.
#'  * `relational` operations, which return a logical array. \cr
#'  For example: ==, !=, etc.
#' 
#' 
#' The 'broadcast' package supports relational operators (==, !=, <, > <=, >=),
#' logical combiners (&, |, xor, nand),
#' arithmetic (+, -, *, /, ^)
#' 
#' 
#' 
#' @author \strong{Author, Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @name aaa00_broadcast_help
#' @rdname aaa00_broadcast_help
#' @aliases broadcast-package
#' @aliases broadcast
#' @aliases broadcast_help
#' @useDynLib broadcast, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' 
NULL
#> NULL
