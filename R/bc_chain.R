#' Evaluate a Chain of Mathematical Operations with Broadcasting
#'
#' @description
#' 
#' The user can supply a formula giving a mathematical (-like) expression in the `bc_chain()` function. \cr
#' Inside `bc_chain()`, the expression is evaluated,
#' where the base operators
#' (+, -, *, /, etc.)
#' have been \bold{overloaded} to use \bold{broadcasting}. \cr
#' I.e. `bc_chain(~ x + y)` is equivalent to `bc.num(x, y, "+")`. \cr
#' What sets `bc_chain()` apart from the other `bc.`-functions,
#' other than the different syntax,
#' is that the user can chain together multiple arithmetic operators,
#' \bold{with the usual mathematical precedence and backeting rules}. \cr
#' For example: \cr
#' `bc_chain(~ (x + y) / z)` \cr
#' \cr
#' 
#'
#' @param f a formula giving the expression to evaluate. \cr
#' The environment of `f` is used to find the variables. \cr
#' If `environment(f)` is `NULL`,
#' the environment from which `bc_chain()` was called
#' is used to find the variables. \cr \cr
#' 
#' 
#' @returns
#' The result from the broadcasted operation. \cr


#' @example inst/examples/bc_chain.R


#' @rdname bc_chain
#' @export
bc_chain <- function(f) {
  
  if(!.is_formula(f)) {
    stop("`f` must be a formula")
  }
  
  out <- eval(
    parse(text = deparse(f[[2]], backtick = TRUE)),
    envir = c(
      bc_overloaded_mathops,
      bc_overloaded_boolops,
      bc_overloaded_relops
    ),
    enclos = .overload_get_env4form(f, parent.frame())
  )
  return(out)
}

