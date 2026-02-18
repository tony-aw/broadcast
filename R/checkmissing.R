#' Efficiently Check for (Non-)Missing Values
#'
#' @description
#' The `checkNULL()` function efficiently checks for the presence
#' (or absence) of `NULL` in every element of a list. \cr
#' The `checkNA()` function efficiently checks for the presence
#' (or absence) of `NA`/`NaN` in every element of an atomic vector. \cr
#' \cr
#' 
#' @param x an object. \cr
#' For `checkNULL()`: a recursive vector or array (i.e. type of `list`). \cr
#' For `checkNA()`: an atomic vector or array (raw type not supported). \cr
#' @param ... further arguments passed to or from methods.
#' @param op a single string, giving the operation to use. \cr
#' The following operations are supported:
#'  - "logical": checks for every element of `x` the presence/absence of missing values; returns a logical vector/array. 
#'  - "raw": same as operation "logical",
#'  except the result will be a raw vector/array (`01` = `TRUE`, `00` = FALSE),
#'  which requires less memory.
#'  - "any": checks if any element is (not) missing.
#'  - "all": checks if all elements are (not) missing.
#'  - "count" or "sum": counts the number of elements that are (not) missing.
#'  - "which": gives the element indices that are (not) missing.
#'  - "first": gives the element index of the first (non-) missing element.
#'  - "last": gives the element index of the last (non-) missing element.
#' @param inv Boolean, indicating if the check should be inverted. \cr
#' If `inv = FALSE` (default), the operations check for missing elements. \cr
#' If `inv = TRUE`, the operations check for NOT missing elements. \cr \cr
#' 
#' 
#' @returns
#' Output depends on the specification of argument `op`:
#'  - "logical": logical vector with the same length, names, and dimensions as `x`.
#'  - "raw": raw vector with the same length, names, and dimensions as `x`.
#'  - "any": `TRUE` or `FALSE`.
#'  - "all": `TRUE` or `FALSE`.
#'  - "count" or "sum": 53 bit integer scalar.
#'  - "which": vector of indices.
#'  - "first": the first index found, or `0` otherwise.
#'  - "last": the last index found, or `0` otherwise.
#'
#'
#' @example inst/examples/checkmissing.R
#' 


#' @name checkmissing
NULL

#' @rdname checkmissing
#' @export
checkNA <- function(x, ...) {
  UseMethod("checkNA", x)
}


#' @rdname checkmissing
#' @export
checkNA.default <- function(x, op, inv = FALSE, ...) {
  
  .ellipsis(list(...), sys.call())
  
  if(!is.atomic(x) || is.raw(x)) {
    stop("unsupported type for `x` given")
  }
  return(.checkmissing(x, op, inv, sys.call()))
}

#' @rdname checkmissing
#' @export
checkNULL <- function(x, ...) {
  UseMethod("checkNULL", x)
}

#' @rdname checkmissing
#' @export
checkNULL.default <- function(x, op, inv = FALSE, ...) {
  
  .ellipsis(list(...), sys.call())
  
  if(!is.list(x)) {
    stop("unsupported type for `x` given")
  }
  return(.checkmissing(x, op, inv, sys.call()))
}


#' @keywords internal
#' @noRd
.checkmissing <- function(x, op, inv = FALSE, abortcall) {
  
  if(!is.character(op) || length(op) != 1L || is.na(op)) {
    stop(simpleError("`op` must be a single non-missing string", call = abortcall))
  }
  if(!isTRUE(inv) && ! isFALSE(inv)) {
    stop(simpleError("`inv` must be `TRUE` or `FALSE`", call = abortcall))
  }
  
  if(op == "any") {
    return(.rcpp_checkmissing_first(x, inv) > 0L)
  }
  else if(op == "all") {
    return(.rcpp_checkmissing_first(x, !inv) == 0L)
  }
  else if(op == "logical") {
    out <- .rcpp_checkmissing_is_logical(x, inv)
    dim(out) <- dim(x)
    dimnames(out) <- dimnames(x)
    names(out) <- names(x)
    broadcaster(out) <- broadcaster(x)
    return(out)
  }
  else if(op == "raw") {
    out <-.rcpp_checkmissing_is_raw(x, inv)
    dim(out) <- dim(x)
    dimnames(out) <- dimnames(x)
    names(out) <- names(x)
    broadcaster(out) <- broadcaster(x)
    return(out)
  }
  else if(op == "count" || op == "sum") {
    return(.rcpp_checkmissing_count(x, inv))
  }
  else if(op == "which") {
    count <- .rcpp_checkmissing_count(x, inv)
    intmax <- 2^31 - 1L
    if(length(x) < intmax) {
      out <- .rcpp_checkmissing_which32(x, inv, count)
    }
    else {
      out <- .rcpp_checkmissing_which64(x, inv, count)
    }
    if(!is.null(names(x)) && length(out) == 0L) {
      names(out) <- character(0L)
    }
    if(!is.null(names(x)) && length(out) > 0L) {
      names(out) <- names(x)[out]
    }
    return(out)
  }
  else if(op == "first") {
    return(.rcpp_checkmissing_first(x, inv))
  }
  else if(op == "last") {
    return(.rcpp_checkmissing_last(x, inv))
  }
  else {
    stop(simpleError("unsupported operator given", call = abortcall))
  }
  
}

