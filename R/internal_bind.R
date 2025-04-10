#

#' @keywords internal
#' @noRd
.bind_input_fix <- function(input, is_dt, abortcall) {
  if(length(input) < 2L) {
    stop(simpleError("`input` must be a list with at least 2 elements", call = abortcall))
  }
  if(!is_dt) {
    input <- input[lengths(input) > 0L]
    if(length(input) == 0L) {
      stop(simpleError("`input` must contain at least one non-zero array/vector", call = abortcall))
    }
  }
  if(is_dt) {
    vnrows <- vapply(input, nrow, integer(1L))
    vncols <- vapply(input, ncol, integer(1L))
    input <- input[vnrows > 0L & vncols > 0L]
    if(length(input) == 0L) {
      stop(simpleError("`input` must contain at least one non-empty data.frame-like object", call = abortcall))
    }
  }
  
  return(input)
}


#' @keywords internal
#' @noRd
.bind_arg_along <- function(
    along, rev, ndim_max, abortcall
) {
  
  if(ndim_max > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  
  if(!isTRUE(rev) && !isFALSE(rev)) {
    stop(simpleError("`rev` must be either `TRUE` or `FALSE`", call = abortcall))
  }
  
  if(!.is.integer_scalar(along)) {
    stop(simpleError("`along` must be an integer scalar", call = abortcall))
  }
  
  if(along < 0L || along > 16L) {
    stop(simpleError("`along` may not be negative or larger than 16", call = abortcall))
  }
  
  if(isTRUE(rev)) {
    N <- ndim_max
    along <- N + 1 - along
  }
  
  
  if(along > (ndim_max + 1L) || along < 0L) { # check < 0L again, since rev was applied
    stop(simpleError("`along` out of bounds", call = abortcall))
  }
  
  return(along)
  
}


#' @keywords internal
#' @noRd
.bind_check_max_ndims <- function(max_ndims, along, abortcall) {
  if(max_ndims > 16L) {
    stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
  }
  if(along == 0L || along == (max_ndims + 1)) {
    if(max_ndims > 15L) {
      stop(simpleError("arrays with more than 16 dimensions are not supported", call = abortcall))
    }
  }
  if(along > (max_ndims + 1L)) {
    stop(simpleError("`along` out of range for the given arrays", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.bind_normalize_dims <- function(input.dims, along, max_ndims) {
  if(along > 0L && along <= max_ndims) {
    which_neednorm <- which(lengths(input.dims) < max_ndims)
    if(length(which_neednorm) > 0L) {
      for(i in which_neednorm) {
        temp <- input.dims[[i]]
        input.dims[[i]] <- c(temp, rep_len(1L, max_ndims - length(temp)))
      }
    }
  }
  else if(along == 0L) {
    for(i in 1:length(input.dims)) {
      temp <- input.dims[[i]]
      input.dims[[i]] <- c(1L, temp, rep_len(1L, max_ndims - length(temp)))
    }
  }
  else if(along == (max_ndims + 1L)) {
    for(i in 1:length(input.dims)) {
      temp <- input.dims[[i]]
      input.dims[[i]] <- c(temp, rep_len(1L, max_ndims - length(temp) + 1L))
    }
  }
  
  return(input.dims)
}



#' @keywords internal
#' @noRd
.internal_bind_array <- function(input, along, ndim2bc, name_along, abortcall) {
  
  INTMAX <- 2^31 - 1L
  LONGMAX <- 2^52 - 1L
  
  # check ndim2bc:
  if(!.is.integer_scalar(ndim2bc)) {
    stop(simpleError("`ndim2bc` must be an integer scalar", call = abortcall))
  }
  ndim2bc <- as.integer(ndim2bc)
  if(ndim2bc < 0) {
    stop(simpleError("`ndim2bc` must be non-negative", call = abortcall))
  }
  
  # remove zero-length arrays
  # NOTE: only remove within this function, as we want to keep them for comnames
  # NOTE: all empty input already covered before running this function
  input <- input[lengths(input) > 0L] 
  
  # make input.dims:
  input.dims <- .rcpp_bindhelper_vdims(input)
  
  
  # check max dims:
  max_ndims <- max(lengths(input.dims))
  .bind_check_max_ndims(max_ndims, along, abortcall)
  
  
  
  # check if extradimensional - MUST do this BEFORE normalizing dims!
  extra_dimensional <- FALSE
  if(along == 0L || along > max_ndims) {
    extra_dimensional <- TRUE
  }
  
  
  # normalize input.dims:
  input.dims <- .bind_normalize_dims(input.dims, along, max_ndims)
  if(along == 0L) along <- 1L
  max_ndims <- max(lengths(input.dims))
  
  
  # get naming params - must do this AFTER normalizing dims!
  if(name_along && !extra_dimensional) {
    # note: dimension `along` never gets broadcasted, so no need to worry about that
    arg.dimnames <- .rcpp_bindhelper_get_dimnames(input, along)
    arg.marginlen <- vapply(input.dims, \(x)(x)[along], integer(1L))
    name_along <- .bind_name_along_reasonable(input, arg.dimnames)
  }
  
  
  # check dimlens:
  dimlens <- lengths(input.dims)
  if(length(unique(dimlens)) > 1L) {
    stop("input malformed")
  }
  max_ndims <- max(dimlens)
  if(max_ndims > 16L) {
    stop(simpleError(
      "arrays with more than 16 dimensions are not supported", call = abortcall
    ))
  }
  
  
  # chunkify input.dims:
  need_pad <- round(max_ndims/2L) != (max_ndims /2L)
  if(need_pad) {
    input.dims <- lapply(input.dims, \(x)c(x, 1L))
  }
  max_ndims <- max(lengths(input.dims))
  
  
  # determine out.dim (padded):
  size_along <- .rcpp_bindhelper_sum_along(input.dims, along - 1L)
  out.dim <- do.call(pmax, input.dims)
  out.dim[along] <- size_along
  out.dim <- as.integer(out.dim)
  out.len <- prod(out.dim)
  if(any(out.dim > INTMAX) || anyNA(out.dim) || out.len > LONGMAX) {
    stop(simpleError("output will exceed maximum vector size", call = abortcall))
  }
  
  
  # check if input is conformable:
  # NOTE: only 1 dimension may be broadcasted per array, for the user's safety
  conf <- .rcpp_bindhelper_conf_dims_all(input.dims, out.dim, along - 1L, ndim2bc)
  if(conf < 0) {
    stop(simpleError("arrays are not conformable for binding", call = abortcall))
  }
  if(conf > ndim2bc) {
    txt <- sprintf(
      "maximum number of dimensions to be broadcasted (%d) exceeds `ndim2bc` (%d)",
      conf, ndim2bc
    )
    stop(simpleError(txt, call = abortcall))
  }
  
  # determine "highest" type:
  out.type <- .rcpp_bindhelper_max_type(input)
  out.type <- .types()[out.type]
  if(out.type == "unknown") {
    stop(simpleError("unknown type of array given", call = abortcall))
  }
  
  # allocate output:
  out <- vector(out.type, out.len)
  if(need_pad) {
    # keep out.dim padded, but don't pad the actual dim(out)
    dim(out) <- out.dim[-length(out.dim)]
  }
  else {
    dim(out) <- out.dim
  }
  
  
  # alias coercion function:
  mycoerce <- .type_alias_coerce(out.type, abortcall)
  
  
  # MAIN FUNCTION:
  counter <- 1L
  max_ndims <- length(out.dim)
  dcp_out <- c(1, cumprod(out.dim))
  for(i in 1:length(input)) {
    
    # construct parameters:
    x <- input[[i]]
    x.dim <- input.dims[[i]]
    size_along <- x.dim[along]
    starts <- rep(1L, max_ndims)
    starts[along] <- counter
    ends <- out.dim
    ends[along] <- counter + size_along - 1L
    by_x <- .C_make_by(x.dim)
    by_x[along] <- 1L
    dcp_x <- c(1, cumprod(x.dim))
    
    # coerce input:
    x <- mycoerce(x)
    
    # pass-by-reference modification:
    .rcpp_bc_bind(out, x, starts - 1L, ends -1L, by_x, dcp_out, dcp_x, out.dim)
    
    # set counter:
    counter <- counter + size_along
  }
  
  
  
  # name_along:
  if(name_along) {
    
    dimnames(out) <- .bind_prep_dimnames(out)
    
    if(!extra_dimensional) {
      dimnames(out)[[along]] <- .bind_get_alongnames(out, along, input, arg.dimnames, arg.marginlen)
    }
    else if(extra_dimensional) {
      if(!is.null(names(input))) {
        dimnames(out)[[along]] <- names(input)
      } else {
        dimnames(out)[[along]] <- paste0("X", seq_len(dim(out)[along]))
      }
    }
  }
  
  return(out)
}

