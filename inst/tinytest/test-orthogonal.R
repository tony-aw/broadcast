# 
# # set-up ====
# enumerate <- 0L
# errorfun <- function(tt) {
#   if(isTRUE(tt)) print(tt)
#   if(isFALSE(tt)) stop(print(tt))
# }
# 
# 
# make_dims_starts <- function(n) {
#   lapply(1:n, \(i) rep(c(sample(2:5, 1L), 1L))) |> unlist()
# }
# make_dims_ends <- function(n) {
#   lapply(1:n, \(i) rep(c(1L, sample(2:5, 1L)))) |> unlist()
# }
# 
# ortho <- broadcast:::.rcpp_bc_dbl_o
# 
# # xstarts ====
# 
# for(i in sample(1:7, 4)) { # dimension 2 to 14 (i.e, 2*1 to 2*7)
#   
#   x.dim <- make_dims_starts(i)
#   y.dim <- make_dims_ends(i)
#   x <- array(sample(1:100), dim = x.dim)
#   y <- array(sample(1:100), dim = y.dim)
#   x.dcp <- c(1, cumprod(x.dim))
#   y.dcp <- c(1, cumprod(y.dim))
#   out.dim <- pmax(x.dim, y.dim) |> as.integer()
#   out.len <- prod(out.dim)
#   
#   expect_true(
#     broadcast:::.C_dims_all_orthogonal(x.dim, y.dim)
#   ) |> errorfun()
#   
#   expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
#   expected[is.nan(expected)] <- NA
#   out <- ortho(x, y, x.dcp, y.dcp, c(0L, 0L), out.dim, out.len, TRUE, 1L)
#   out[is.nan(out)] <- NA
#   dim(out) <- out.dim
#   expect_equal(
#     out, expected
#   ) |> errorfun()
#   enumerate <- enumerate + 2L
# }
# 
# 
# # ystarts ====
# 
# for(i in sample(1:7, 4)) { # dimensions2 to 14 (i.e, 2*1 to 2*7)
#   
#   x.dim <- make_dims_ends(i)
#   y.dim <- make_dims_starts(i)
#   x <- array(sample(1:100), dim = x.dim)
#   y <- array(sample(1:100), dim = y.dim)
#   x.dcp <- c(1, cumprod(x.dim))
#   y.dcp <- c(1, cumprod(y.dim))
#   out.dim <- pmax(x.dim, y.dim) |> as.integer()
#   out.len <- prod(out.dim)
#   
#   expect_true(
#     broadcast:::.C_dims_all_orthogonal(x.dim, y.dim)
#   ) |> errorfun()
#   
#   expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
#   expected[is.nan(expected)] <- NA
#   out <- ortho(x, y, x.dcp, y.dcp, c(0L, 0L), out.dim, out.len, FALSE, 1L)
#   out[is.nan(out)] <- NA
#   dim(out) <- out.dim
#   expect_equal(
#     out, expected
#   ) |> errorfun()
#   enumerate <- enumerate + 2L
# }
# 
# 
# 
# # xstarts, sandwiched ====
# 
# for(i in 4:8) {
#   for(first in c(TRUE, FALSE)) {
#     for(last in c(TRUE, FALSE)) {
#       
#       n <- sample(3:5, 1L)
#       x.dim <- rep_len(c(n, 1L), i)
#       y.dim <- rep_len(c(1L, n - 1L), i)
#       by_first_last <- c(first, last) |> as.integer()
#       if(first) {
#         y.dim[1] <- n
#       }
#       if(last) {
#         x.dim[i] <- n
#         y.dim[i] <- n
#       }
#       x <- array(sample(1:100), dim = x.dim)
#       y <- array(sample(1:100), dim = y.dim)
#       
#       expect_true(
#         broadcast:::.is_sandwich_orthogonal(x.dim, y.dim)
#       )
#       
#       by_x <- ifelse(dim(x) > 1L, 1L, 0L)
#       by_y <- ifelse(dim(y) > 1L, 1L, 0L)
#       x.dcp <- c(1, cumprod(x.dim))
#       y.dcp <- c(1, cumprod(y.dim))
#       out.dim <- pmax(x.dim, y.dim) |> as.integer()
#       out.len <- prod(out.dim)
#       
#       expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
#       expected[is.nan(expected)] <- NA
#       out <- ortho(x, y, x.dcp, y.dcp, by_first_last, out.dim, out.len, TRUE, 1L)
#       out[is.nan(out)] <- NA
#       dim(out) <- out.dim
#       expect_equal(
#         out, expected
#       ) |> errorfun()
#       enumerate <- enumerate + 2L
#     }
#   }
# }
# 
# 
# # ystarts, sandwiched ====
# 
# for(i in 4:8) {
#   for(first in c(TRUE, FALSE)) {
#     for(last in c(TRUE, FALSE)) {
#       
#       n <- sample(3:5, 1L)
#       x.dim <- rep_len(c(1L, n - 1L), i)
#       y.dim <- rep_len(c(n, 1L), i)
#       by_first_last <- c(first, last) |> as.integer()
#       if(first) {
#         x.dim[1] <- n
#       }
#       if(last) {
#         y.dim[i] <- n
#         x.dim[i] <- n
#       }
#       x <- array(sample(1:100), dim = x.dim)
#       y <- array(sample(1:100), dim = y.dim)
#       
#       expect_true(
#         broadcast:::.is_sandwich_orthogonal(x.dim, y.dim)
#       )
#       
#       by_x <- ifelse(dim(x) > 1L, 1L, 0L)
#       by_y <- ifelse(dim(y) > 1L, 1L, 0L)
#       x.dcp <- c(1, cumprod(x.dim))
#       y.dcp <- c(1, cumprod(y.dim))
#       out.dim <- pmax(x.dim, y.dim) |> as.integer()
#       out.len <- prod(out.dim)
#       
#       expected <- array_recycle(x, out.dim) + array_recycle(y, out.dim)
#       expected[is.nan(expected)] <- NA
#       out <- ortho(x, y, x.dcp, y.dcp, by_first_last, out.dim, out.len, FALSE, 1L)
#       out[is.nan(out)] <- NA
#       dim(out) <- out.dim
#       expect_equal(
#         out, expected
#       ) |> errorfun()
#       enumerate <- enumerate + 2L
#     }
#   }
#   
#   
# }
