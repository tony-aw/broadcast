#' 
#' 
#' 
#' `==.broadcaster` <- function(e1, e2) {
#'   .binary_stop_general(e1, e2, "==", sys.call())
#'   if(is.character(e1) && is.character(e2)) {
#'     return(.bc_str_rel(e1, e2, 1L, sys.call()))
#'   }
#'   else if(is.complex(e1) || is.complex(e2)) {
#'     if(!is.complex(e1)) e1 <- as_cplx(e1)
#'     if(!is.complex(e2)) e2 <- as_cplx(e2)
#'     return(.bc_cplx_rel(e1, e2, 1L, sys.call()))
#'   }
#'   else if(.is_numeric_like(e1) && .is_numeric_like(e2)) {
#'     return(.bc_dec_rel(e1, e2, 1L, sys.call()))
#'   }
#'   else {
#'     stop("unsupported combination of types given")
#'   }
#' }
