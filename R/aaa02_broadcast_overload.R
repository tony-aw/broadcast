#' Overloaded Operators
#' 
#' @description
#' Sometimes broadcasting is needed in large mathematical expression,
#' involving multiple variables,
#' where precedence is of importance. \cr
#' For example in an expression like `x + y / z^y`. \cr
#' For such cases, you may want to overload the base operators. \cr
#' \cr
#' To that end, the 'broadcast' package provides the \link{broadcaster} class,
#' which comes with its own method dispatch for the base operators. \cr
#' If at least one of the 2 arguments of the base operators has the `broadcaster` class attribute,
#' and no other class (like `bit64`) interferes,
#' broadcasting will occur in the same manner as used in the various `bc.` - functions. \cr
#' \cr
#' The following arithmetic operators have a 'broadcaster' method:
#' +, -, *, /, ^, %%, %/% \cr
#' The following relational operators have a 'broadcaster' method:
#' ==, !=, <, >, <=, >= \cr
#' And finally, the & and | operators also have a 'broadcaster' method. \cr
#' \cr
#' The overloaded operators mimic the behaviour of the base 'R' operators accurately,
#' except they employ broadcasting, and they generally do not preserve attributes
#' (see \link{broadcast_attributes} for details on how the operators from 'broadcast' handle attributes). \cr
#' \cr
#' \cr
#' @example inst/examples/broadcaster.R
#' 
#' 
#' 
#' @name aaa02_broadcast_overload
#' @rdname aaa02_broadcast_overload
#' @aliases broadcast_overload
#' 
NULL
#> NULL
