
#' @keywords internal
#' @noRd
.bind_input_fix <- function(input, abortcall) {
  if(!is.list(input)) {
    stop(simpleError("`input` must be a list", call = abortcall))
  }
  if(.C_any_nonarray(input)) {
    stop(simpleError("can only bind arrays", call = abortcall))
  }
  if(length(input) < 2L) {
    stop(simpleError("`input` must be a list with at least 2 elements", call = abortcall))
  }
  if(length(input) > (2L^16L)) {
    stop(simpleError("too many objects given in `input`", call = abortcall))
  }
  input <- input[lengths(input) > 0L]
  if(length(input) == 0L) {
    stop(simpleError("`input` must contain at least one non-zero array/vector", call = abortcall))
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
.bind_normalize_dims <- function(input.dims, dimlens, along, max_ndims) {
  if(along > 0L && along <= max_ndims) {
    return(.rcpp_normalize_dims(input.dims, 0L, max_ndims))
  }
  else if(along == 0L) {
    return(.rcpp_normalize_dims(input.dims, 1L, max_ndims + 1L))
  }
  else if(along == (max_ndims + 1L)) {
    return(.rcpp_normalize_dims(input.dims, 0L, max_ndims + 1L))
  }
}



#' @keywords internal
#' @noRd
.internal_bind_array <- function(input, along, ndim2bc, name_along, abortcall) {
  
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
  input.dims <- .C_bindhelper_vdims(input)
  dimlens <- lengths(input.dims)
  
  
  # check max ndims:
  max_ndims <- max(dimlens)
  .bind_check_max_ndims(max_ndims, along, abortcall)
  
  
  # check if extradimensional - MUST do this BEFORE normalizing dims!
  extra_dimensional <- FALSE
  if(along == 0L || along > max_ndims) {
    extra_dimensional <- TRUE
  }
  
  
  # normalize input.dims:
  input.dims <- .bind_normalize_dims(input.dims, dimlens, along, max_ndims)
  if(along == 0L) along <- 1L
  max_ndims <- length(input.dims[[1L]])
  
  
  # get naming params - must do this AFTER normalizing dims!
  if(name_along && !extra_dimensional) {
    # note: dimension `along` never gets broadcasted, so no need to worry about that
    arg.dimnames <- .rcpp_bindhelper_get_dimnames(input, along)
    arg.marginlen <- .C_bindhelper_get_alongdims(input.dims, along - 1L)
    name_along <- .bind_name_along_reasonable(input, arg.dimnames)
  }
  
  
  # check dimlens:
  if(max_ndims > 16L) {
    stop(simpleError(
      "arrays with more than 16 dimensions are not supported", call = abortcall
    ))
  }
  
  # determine original out.dim:
  out.dimorig <- do.call(pmax, input.dims)
  
  # chunkify input.dims:
  input.dims <- lapply(input.dims, .C_chunkify_dims, chunks = c(4L, 16L))
  max_ndims <- length(input.dims[[1L]])
  
  # determine out.dim (padded):
  size_along <- .C_bindhelper_sum_along(input.dims, along - 1L)
  out.dim <- do.call(pmax, input.dims)
  out.dim[along] <- size_along
  out.dimorig[along] <- size_along # original dimensions
  out.dim <- as.integer(out.dim)
  out.len <- prod(out.dim)
  if(.C_arraysize_overflow(out.dim, out.len)) {
    stop(simpleError("output will exceed maximum vector size", call = abortcall))
  }
  
  
  # check if input is conformable:
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
  out.type <- .C_bindhelper_max_type(input)
  out.type <- .types()[out.type]
  if(out.type == "unknown") {
    stop(simpleError("unknown type of array given", call = abortcall))
  }
  
  # allocate output:
  out <- vector(out.type, out.len)
  dim(out) <- out.dimorig
  
  
  # alias coercion function:
  mycoerce <- .type_alias_coerce(out.type, abortcall)
  need_coerce <- .C_bindhelper_need_coerce(input, out)
  
  
  # MAIN FUNCTION:
  counter <- 0L
  max_ndims <- length(out.dim)
  dcp_out <- .C_make_dcp(out.dim)
  starts <- .rcpp_clone(rep(0L, max_ndims))
  ends <- .rcpp_clone(out.dim)
  by_x <- vector("integer", max_ndims)
  dcp_x <- vector("double", max_ndims+1)
  size_along <- 0L
  
  for(i in 1:length(input)) {
    
    # construct parameters:
    x <- input[[i]]
    x.dim <- input.dims[[i]]
    size_along <- x.dim[along]
    
    
    # coerce input if necessary:
    if(need_coerce[i]) {
      x <- mycoerce(x)
    }
    
    
    # pass-by-reference modification:
    .rcpp_bc_bind_prep(
      starts, ends, by_x, dcp_x, x.dim, out.dim, along - 1L, size_along, counter, max_ndims
    )
    
    .rcpp_bc_bind(out, x, starts, ends, by_x, dcp_out, dcp_x, out.dim)
    
    
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

