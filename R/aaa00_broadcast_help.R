#' broadcast: Simple Broadcasted Operations for Atomic and Recursive Arrays with Minimal Dependencies
#' 
#' @description
#' broadcast: \cr
#' Simple Broadcasted Binding and Binary Operations for Atomic and Recursive Arrays with Minimal Dependencies. \cr \cr
#' 
#' ```{r echo = FALSE, eval = TRUE, results = 'asis'}
#' 
#' txt <- packageDescription("broadcast", fields = "Description")
#' p <- c("\t", ",\n", ".\n", "\n(", "following.")
#' rp <- c("", ", ", ".\n\n",  " (", "following:")
#' for(i in 1:length(rp)) {
#'  txt <- gsub(p[i], rp[i], txt, fixed = TRUE)
#' }
#' cat(txt)
#' ```
#' 
#' @section Getting Started:
#' An introduction and overview of the package can be found on the website. \cr
#' \cr
#' 
#' @section Methods and Functions:
#' 
#' \bold{Type Specific Binary Operations} \cr
#' 'broadcast' provides a set of functions for type-specific binary operations
#' for broadcasted operations. \cr
#' These functions use an API similar to the \link[base]{outer} and \link[base]{sweep} functions. \cr
#' \cr
#' The following functions for type-specific binary operations are available:
#' 
#'  * \link{bc.d}: numeric arithmetic and relational operations;
#'  * \link{bc.b}: Boolean combiner operations;
#'  * \link{bc.cplx}: complex arithmetic and equality operations;
#'  * \link{bc.str}: string equality, concatenation, and distance operations;
#'  * \link{bc.list}: apply any 'R' function to 2 recursive arrays with broadcasting. \cr \cr
#' 
#' \bold{General functions} \cr
#' 'broadcast' also comes with 2 general broadcasted functions:
#' 
#'  * \link{bc_ifelse}: Broadcasted version of \link[base]{ifelse}.
#'  * \link{bcapply}: Broadcasted apply-like function. \cr \cr
#'  
#' \bold{Binding Implementations} \cr
#' 'broadcast' provides 3 binding implementations: \cr
#' \link{bind_mat}, \link{bind_array}, and \link{bind_dt}. \cr
#' \cr
#' \cr
#' \bold{Other functions} \cr
#' 'broadcast' also provides
#' \link[=as_bool]{type-casting} functions,
#' which preserve names and dimensions - convenient for arrays. \cr \cr
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
