#' broadcast Package Overview
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
#' An introduction and overview of the package can be found
#' \href{https://tony-aw.github.io/broadcast/vignettes/a_readme.html}{HERE}. \cr
#' Note that 'broadcast' is still somewhat experimental;
#' if you find bugs or other issues,
#' please report them promptly on the 'broadcast' GitHub
#' \href{https://github.com/tony-aw/broadcast/issues}{issues tab}. \cr
#' \cr
#' 
#' @section Functions:
#' 
#' \bold{Functions for broadcasted element-wise binary operations} \cr
#' 'broadcast' provides a set of functions for broadcasted element-wise binary operations
#' with broadcasting. \cr
#' These functions use an API similar to the \link[base]{outer} function. \cr
#' \cr
#' The following functions for type-specific binary operations are available:
#' 
#'  * \link{bc.b}: Boolean (i.e. logical) operations;
#'  * \link{bc.i}: integer arithmetic and relational operations;
#'  * \link{bc.d}: decimal arithmetic and relational operations;
#'  * \link{bc.cplx}: complex arithmetic and (in)equality operations;
#'  * \link{bc.str}: string (in)equality, concatenation, and distance operations;
#'  * \link{bc.raw}: byte- and relational operations for vectors/arrays of type `raw`;
#'  * \link{bc.bit}: BIT-WISE operations, supporting the `raw` and `integer` types;
#'  * \link{bc.list}: apply any 'R' function to 2 recursive arrays with broadcasting. \cr \cr
#' 
#' 
#' \bold{\code{bind_array()}} \cr
#' 'broadcast' provides the \link{bind_array} function,
#' to bind arrays along an arbitrary dimension,
#' with support for broadcasting. \cr
#' \cr
#' The API of `bind_array()` is inspired by the fantastic
#' \code{abind::abind()} function
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
#' 
#' \bold{General functions} \cr
#' 'broadcast' also comes with 2 general broadcasted functions:
#' 
#'  * \link{bc_ifelse}: Broadcasted version of \link[base]{ifelse}.
#'  * \link{bcapply}: Broadcasted apply-like function. \cr \cr
#' 
#' 
#' \bold{Other functions} \cr
#' 'broadcast' provides the \link{acast} function,
#' for casting (i.e. pivoting) an array into a new dimension. \cr
#' \cr
#' 'broadcast' also provides
#' \link[=as_bool]{type-casting} functions,
#' which preserve names and dimensions - convenient for arrays. \cr \cr
#' 
#' 
#' @section Overloading:
#' Sometimes broadcasting is needed in large mathematical expression,
#' involving multiple variables,
#' where precedence is of importance. \cr
#' For example in an expression like `(x + y) / z^y`. \cr
#' For such cases, you may want to overload the base operators. \cr
#' \cr
#' The 'broadcast' package
#' provides 2 ways to overload base operators to support broadcasting:
#' 
#'  1) Via the \link{bc_chain} function,
#'  to evaluate a mathematical expression using overloaded operators for broadcast support.
#'  2) Via the \link{broadcaster} class,
#'  which comes with its own method dispatch for the base operators. \cr
#'
#' Overloads for the relational operators (==, !=, etc.) have also been implemented. \cr
#' Please refer to the website for additional details. \cr \cr
#' 
#' 
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#' 
#' @author \strong{Author, Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
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
