
.op_rel <- function(op, abortcall) {
  op <- trimws(op, which = "both")
  if(op == "==") {
    return(1L)
  }
  else if(op == "!=") {
    return(2L)
  }
  else if(op == "<") {
    return(3L)
  }
  else if(op == ">") {
    return(4L)
  }
  else if(op == "<="){
    return(5L)
  }
  else if(op == ">=") {
    return(6L)
  }
  else {
    stop(simpleError("given operator not supported in the given context"))
  }
}

#' @keywords internal
#' @noRd
.op_dbl <- function(op, abortcall) {
  op <- trimws(op, which = "both")
  if(op == "+") {
    return(1L)
  }
  else if(op == "-") {
    return(2L)
  }
  else if(op == "*") {
    return(3L)
  }
  else if(op == "/") {
    return(4L)
  }
  else if(op == "^"){
    return(5L)
  }
  else if(op == "pmin") {
    return(6L)
  }
  else if(op == "pmax") {
    return(7L)
  }
  else {
    stop(simpleError("given operator not supported in the given context"))
  }
}
