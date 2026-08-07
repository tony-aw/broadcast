#


#' @keywords internal
#' @noRd
.binary_stop_general <- function(x, y, op, abortcall) {
  if(!.is_array_like(x) || !.is_array_like(y)) {
    stop(simpleError("input must be arrays or simple vecors", call = abortcall))
  }
  if(ndim(x) > 16L || ndim(y) > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  if(!is.character(op) || length(op) != 1L) {
    stop(simpleError("`op` must be single string", call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.binary_prep <- function(x, y) {
  
  x.ndim <- ndim(x)
  y.ndim <- ndim(y)
  out <- .rcpp_virt_binary_prep(x, y, x.ndim, y.ndim)
  names(out) <- c("x.dim", "y.dim", "out.dimorig", "out.dimsimp", "out.len", "dimmode")
  return(out)
  
}




#' @keywords internal
#' @noRd
.binary_determine_out.len <- function(out.dim, x.len, y.len) {
  if(is.null(out.dim)) {
    return(max(x.len, y.len))
  }
  else {
    return(prod(out.dim))
  }
  
}



#' @keywords internal
#' @noRd
.binary_set_attr <- function(out, x, y) {
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  
  if(is.atomic(out) && (inherits(x, "mutatomic") || inherits(y, "mutatomic"))) {
    if(inherits(x, "mutatomic") && !is.null(attr(x, "serial"))) {
      .rcpp_set_attr(out, "class", c("mutatomic", oldClass(out)))
      .rcpp_set_attr(out, "serial", attr(x, "serial"))
    }
    else if(inherits(y, "mutatomic") && !is.null(attr(y, "serial"))) {
      .rcpp_set_attr(out, "class", c("mutatomic", oldClass(out)))
      .rcpp_set_attr(out, "serial", attr(y, "serial"))
    }
  }
  
  .rcpp_binames_set(x, y, out)
  .binary_set_comm(x, y, out)
  
}

#' @keywords internal
#' @noRd
.binary_set_attr_logical <- function(out, x, y) {
  
  if(inherits(x, "broadcaster") || inherits(y, "broadcaster")) {
    .rcpp_set_attr(out, "class", "broadcaster")
  }
  
  .rcpp_binames_set(x, y, out)
  
}



#' @keywords internal
#' @noRd
.binary_return_zerolen <- function(x, y, is_logical_op = FALSE, returntype = NULL) {
  
  # determine output type & and make output of type:
  out.type <- returntype
  if(is.null(returntype)) {
    out.type <- .C_max_type(list(x, y))
    out.type <- .types()[out.type]
  }
  out <- vector(out.type, 0L)
  
  # length INDEPENDENT attributes:
  if(broadcaster(x) || broadcaster(y)) {
    broadcaster(out) <- TRUE
  }
  
  if(!is_logical_op) {
    .binary_set_comm(x, y, out)
  }
  
  
  return(out)
  
}


#' @keywords internal
#' @noRd
.binary_set_comm <- function(x, y, out) {
  x.comm <- comment(x)
  y.comm <- comment(y)
  x.hascom <- !is.null(x.comm)
  y.hascom <- !is.null(y.comm)
  if(x.hascom != y.hascom) {
    if(x.hascom) {
      .rcpp_set_attr(out, "comment", x.comm)
    }
    else {
      .rcpp_set_attr(out, "comment", y.comm)
    }
  }
}
