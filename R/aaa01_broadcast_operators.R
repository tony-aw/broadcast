#' Details on Broadcasted Operators
#' 
#' @description
#' 
#' Base 'R' comes with relational,
#' arithmetic, integer, logical, and bit-wise operators. \cr
#' 'broadcast' provides 2 ways to use these operators with broadcasting. \cr
#' \cr
#' The first (and simple) way is to use the \link{broadcaster} class,
#' which comes with it's own method dispatch for the above mentioned operators. \cr
#' This approach supports operator precedence, and for the average 'R' user,
#' this is sufficient. \cr
#' \cr
#' The second way is to use the large set of `bc.`- functions. \cr
#' These offer much greater control and more operators than the previous method,
#' and has less risk of running into conflicting methods. \cr
#' But they do not support operator precedence. \cr
#' \cr
#' 
#' 
#' @section Operators Overloaded via Broadcaster Class: 
#' The 'broadcast' package provides the \link{broadcaster} class,
#' which comes with its own method dispatch for the base operators. \cr
#' If at least one of the 2 arguments of the base operators has the \link{broadcaster} class attribute,
#' and no other class (like `bit64`) interferes,
#' broadcasting will be used. \cr
#' \cr
#' The following operators have a 'broadcaster' method:
#' 
#' ```{r, eval = FALSE, echo = TRUE}
#' # Group      : Operators             ; Comments
#' # relational : ==, !=, <, >, <=, >=  ;
#' # arithmetic : +, -, *, /, ^         ;
#' # integer    : %%, %/%               ; both sides will be truncated
#' # logical    : &, |                  ;
#' # bit-wise   : &, |                  ; if both sides are type of `raw`
#' 
#' ```
#' 
#' See also the Examples section below. \cr
#' Unary operations (i.e. `+ x`, `- x`) return the original object,
#' with only the sign adjusted. \cr
#' \cr
#' 
#' 
#' @section Available `bc.` functions: 
#' 'broadcast' provides a set of functions for broadcasted element-wise binary operations
#' with broadcasting. \cr
#' These functions use an API similar to the \link[base]{outer} function. \cr
#' \cr
#' The following functions for simple operations are available:
#' 
#'  * \link{bc.rel}: General relational operations
#'  (`==, !=, <, >, <=, >=`). \cr
#'  The various other `bc.` methods below have specialised relational operators available for specialised needs.
#'  * \link{bc.b}: Boolean/logical operations;
#'  * \link{bc.i}: integer operations;
#'  * \link{bc.d}: decimal arithmetic operations;
#'  * \link{bc.cplx}: complex arithmetic operations;
#'  * \link{bc.str}: string (in)equality, concatenation, and distance operations;
#'  * \link{bc.raw}: operations that take in arrays of type `raw` and return an array of type `raw`;
#'  * \link{bc.bit}: BIT-WISE operations, supporting the `raw` and `integer` types;
#'  * \link{bc.list}: apply any 'R' function to 2 recursive arrays with broadcasting. \cr \cr
#'  
#' 
#' @example inst/examples/broadcaster.R
#' 
#' 
#' @name aaa01_broadcast_operators
#' @rdname aaa01_broadcast_operators
#' @aliases broadcast_operators
#' 
NULL
#> NULL
