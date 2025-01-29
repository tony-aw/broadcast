#

#' @keywords internal
#' @noRd
.prep_binary <- function(x, y, abortcall) {
  
  x.dim <- dim(x)
  y.dim <- dim(y)
  x.len <- length(x)
  y.len <- length(y)
  if(is.null(x.dim)) x.dim <- x.len
  if(is.null(y.dim)) y.dim <- y.len
  
  # normalize dimensions:
  prep <- .normalize_dims(x.dim, y.dim)
  x.dim <- prep[[1L]]
  y.dim <- prep[[2L]]
  
  
  # Check & determine dimensions to return:
  .stop_conf_dim(x.dim, y.dim, x.len, y.len, abortcall)
  out.dimorig <- .determine_out.dim(x.dim, y.dim, abortcall)
  out.len <- .determine_out.len(x.dim, y.dim, x.len, y.len, out.dimorig)
  
  
  # Simplify arrays, to reduce broadcast load:
  simp <- .simplify_dims(x.dim, y.dim, x.len, y.len)
  x.dim <- simp[[1L]]
  y.dim <- simp[[2L]]
  out.dimsimp <- .determine_out.dim(x.dim, y.dim, abortcall)
  
  # Determine type of dimensional relationship for broadcasting:
  dimmode <- .determine_dimmode(x.dim, y.dim, x.len, y.len, out.dimsimp, abortcall)
  
  out <- list(
    x.dim = x.dim,
    y.dim = y.dim,
    x.len = x.len,
    y.len = y.len,
    out.dimorig = out.dimorig,
    out.dimsimp = out.dimsimp,
    out.len = out.len,
    dimmode = dimmode
  )
  
  return(out)
  
}
