#


#' @keywords internal
#' @noRd
.bind_check_args <- function(
    along, name_along, comnames_from, name_flat, abortcall
) {
  
  if(!is.numeric(along) || length(along) != 1) {
    stop(simpleError("`along` must be an integer scalar", call = abortcall))
  }
  if(along < 0L || along > 16L) {
    stop(simpleError("`along` may not be negative or larger than 16", call = abortcall))
  }
  
  if(!is.logical(name_along) || length(name_along) != 1) {
    stop(simpleError("`name_along` must be a Boolean", call = abortcall))
  }
  
  if(!is.null(comnames_from)) {
    if(!is.numeric(comnames_from) || length(comnames_from) != 1) {
      stop(simpleError("`comnames_from` must be an integer scalar or `NULL`", call = abortcall))
    }
  }
  
  if(!is.logical(name_flat) || length(name_flat) != 1) {
    stop(simpleError("`name_flat` must be a Boolean", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.bind_check_input <- function(input, along, max_ndims, abortcall) {
  if(max_ndims > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  if(along == 0L || along == (max_ndims + 1)) {
    if(max_ndims > 15L) {
      stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
    }
  }
  if(along > (max_ndims + 1L)) {
    stop(simpleError("`along` too large for the given arrays", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.bind_normalize_input <- function(input, along, max_ndims) {
  if(along > 0L && along <= max_ndims) {
    which_neednorm <- which(.rcpp_bindhelper_neednorm(input, max_ndims))
    if(length(which_neednorm) > 0L) {
      for(i in which_neednorm) {
        input.dim <- dim(input[[i]])
        dim(input[[i]]) <- c(input.dim, rep_len(1L, max_ndims - length(input.dim)))
      }
    }
  }
  else if(along == 0L) {
    for(i in 1:length(input)) {
      input.dim <- dim(input[[i]])
      dim(input[[i]]) <- c(1L, input.dim, rep_len(1L, max_ndims - length(input.dim)))
    }
  }
  else if(along == (max_ndims + 1L)) {
    for(i in 1:length(input)) {
      input.dim <- dim(input[[i]])
      dim(input[[i]]) <- c(input.dim, rep_len(1L, max_ndims - length(input.dim) + 1L))
    }
  }
  
  return(input)
}

#' @keywords internal
#' @noRd
.bind_alias_coerce <- function(out.type, abortcall) {
  if(out.type == "list") {
    coerce <- as.list
  }
  else if(out.type == "character") {
    coerce <- as.character
  }
  else if(out.type == "complex") {
    coerce <- as.complex
  }
  else if(out.type == "double") {
    coerce <- as.double
  }
  else if(out.type == "integer") {
    coerce <- as.integer
  }
  else if(out.type == "logical") {
    coerce <- as.logical
  }
  else if(out.type == "raw") {
    coerce <- as.raw
  }
  else {
    stop(simpleError("unknown type"))
  }
  return(coerce)
}


#' @keywords internal
#' @noRd
.internal_bind_array <- function(input, along, max_bc, abortcall) {
  
  INTMAX <- 2^31 - 1L
  LONGMAX <- 2^52 - 1L
  
  # check input:
  all_arrays <- vapply(input, is.array, logical(1L)) |> all()
  if(!all_arrays) {
    stop(simpleError("can only bind arrays", call = abortcall))
  }
  max_ndims <- max(.rcpp_bindhelper_dimlens(input))
  .bind_check_input(input, along, max_ndims, abortcall)
  
  
  # normalize input:
  input <- .bind_normalize_input(input, along, max_ndims)
  if(along == 0L) along <- 1L
  max_ndims <- max(.rcpp_bindhelper_dimlens(input))
  
  
  # check dimlens:
  dimlens <- .rcpp_bindhelper_dimlens(input)
  if(length(unique(dimlens)) > 1L) {
    stop("input malformed")
  }
  max_ndims <- max(dimlens)
  if(max_ndims > 16L) {
    stop(simpleError(
      "arrays with more than 16 dimensions are not supported", call = abortcall
    ))
  }
  
  # determine dim(out):
  input.dims <- lapply(input, dim)
  size_along <- .rcpp_bindhelper_sum_along(input, along - 1L)
  out.dim <- do.call(pmax, input.dims)
  out.dim[along] <- size_along
  out.dim <- as.integer(out.dim)
  out.len <- prod(out.dim)
  if(any(out.dim > INTMAX) || anyNA(out.dim) || out.len > LONGMAX) {
    stop(simpleError("output too large to allocate", call = abortcall))
  }
  
  # check if input is conformable:
  # NOTE: only 1 dimension may be broadcasted per array, for the user's safety
  is_conf <- .rcpp_bindhelper_conf_dims_all(input.dims, out.dim, along - 1L, max_bc)
  if(!is_conf) {
    stop(simpleError("arrays are not conformable for binding", call = abortcall))
  }
  
  # determine "highest" type:
  out.type <- .rcpp_bindhelper_max_type(input)
  if(out.type == "unknown") {
    stop(simpleError("unknown type of array given", call = abortcall))
  }
  
  # allocate output:
  out <- vector(out.type, out.len)
  dim(out) <- out.dim
  
  # alias coercion function:
  mycoerce <- .bind_alias_coerce(out.type, abortcall)
  
  # MAIN FUNCTION:
  need_pad <- round(max_ndims/2L) != (max_ndims /2L)
  if(need_pad) {
    out.dim <- c(out.dim, 1L)
  }
  counter <- 1L
  max_ndims <- length(out.dim)
  dcp_out <- c(1, cumprod(out.dim))
  for(i in 1:length(input)) {
    
    # construct parameters:
    x <- input[[i]]
    x.dim <- c(dim(x), 0L) # padding is safe even when not needed
    size_along <- x.dim[along]
    starts <- rep(1L, max_ndims)
    starts[along] <- counter
    ends <- out.dim
    ends[along] <- counter + size_along - 1L
    by_x <- .make_by(x.dim)
    by_x[along] <- 1L
    dcp_x <- c(1, cumprod(x.dim)) # is already longer than needed, so no padding required
    
    # coerce input:
    x <- mycoerce(x)
    
    # pass-by-reference modification:
    rcpp_bc_bind(out, x, starts - 1L, ends -1L, by_x, dcp_out, dcp_x, out.dim)
    
    # set counter:
    counter <- counter + size_along
  }
  
  return(out)
}
