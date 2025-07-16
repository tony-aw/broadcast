
#' @rdname bind_array
#' @export
bind_array <- function(
    input, along, rev = FALSE, ndim2bc = 16L, name_along = TRUE, comnames_from = 1L
) {
  
  if(!is.list(input) && is.array(along)) {
    stop("did you forget to put all input arrays into a single list for `input`?")
  }
  
  # input fix:
  input2 <- .bind_input_fix(input, sys.call())
  ndim_max <- max(lst.ndim(input2))
  
  # along fix:
  # check (rev)along:
  along <- .bind_arg_along(along, rev, ndim_max, sys.call())
  
  
  # naming argument checks:
  .bind_stop_name_along(name_along, abortcall = sys.call())
  .bind_stop_comnames_from(comnames_from, input, abortcall = sys.call())
  
  
  # return original:
  if(length(input2) == 1L) {
    return(input2[[1L]])
  }

  # main function:
  out <- .internal_bind_array(input2, along, ndim2bc, name_along, sys.call())
  
  
  # add comnames:
  if(!is.null(comnames_from)) {
    if(.bind_comnames_reasonable(input, along, comnames_from, ndim_max)) {
      
      obj <- input[[comnames_from]]
      bindwhich <- .bind_which_comnames(out, along, obj, ndim_max)
      out.ind <- bindwhich[[1L]]
      obj.ind <- bindwhich[[2L]]
      
      if(!is.null(out.ind) && !is.null(obj.ind)) {
        dimnames(out) <- .bind_prep_dimnames(out)
        dimnames(out)[out.ind] <- dimnames(obj)[obj.ind]
      }
    }
    
  }
  
  
  # remove dimnames if not necessary (probably not needed, but just in case)
  if(!is.null(dimnames(out))) {
    if(!.C_any_nonNULL(dimnames(out))) {
      dimnames(out) <- NULL
    }
  }
  
  # return output:
  return(out)
}

