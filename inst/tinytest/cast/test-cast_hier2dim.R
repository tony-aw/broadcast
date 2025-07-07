

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist


basefun3 <- function(x, in2out = TRUE) {
  dims <- broadcast:::.hiercast_dims(x, 3L, in2out, FALSE)
  if(in2out) {
    out <- vector("list", prod(dims))
    dim(out) <- dims
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        for(k in 1:dims[3]) {
          temp <- tryCatch({x[[c(k, j, i)]]}, error = function(cond)return(NULL))
          if(is.null(temp)) {
            out[i, j, k] <- list(NULL)
          }
          else {
            out[[i, j, k]] <- temp
          }
        }
      }
    }
  }
  else {
    out <- vector("list", prod(dims))
    dim(out) <- dims
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        for(k in 1:dims[3]) {
          temp <- tryCatch({x[[c(i, j, k)]]}, error = function(cond)return(NULL))
          if(is.null(temp)) {
            out[i, j, k] <- list(NULL)
          }
          else {
            out[[i, j, k]] <- temp
          }
          
        }
      }
    }
  }
  return(out)
}

basefun2 <- function(x, in2out = TRUE) {
  dims <- broadcast:::.hiercast_dims(x, 2L, in2out, FALSE)
  if(in2out) {
    out <- vector("list", prod(dims))
    dim(out) <- dims
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        temp <- tryCatch({x[[c(j, i)]]}, error = function(cond)return(NULL))
        if(is.null(temp)) {
          out[i, j] <- list(NULL)
        }
        else {
          out[[i, j]] <- temp
        }
      }
    }
  }
  else {
    out <- vector("list", prod(dims))
    dim(out) <- dims
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        temp <- tryCatch({x[[c(i, j)]]}, error = function(cond)return(NULL))
        if(is.null(temp)) {
          out[i, j] <- list(NULL)
        }
        else {
          out[[i, j]] <- temp
        }
      }
    }
  }
  return(out)
}



# main function test - simple lists ====
x <- list(as.list(1:10))

expected <- as.list(1:10)
dim(expected) <- c(10, 1)
expect_equal(
  cast_hier2dim(x),
  expected
)

expected <- as.list(1:10)
dim(expected) <- c(1, 10)
expect_equal(
  cast_hier2dim(x, in2out = FALSE),
  expected
)

enumerate <- enumerate + 2L



# main function test general lists ====

x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB", list(NULL)),
    A = list(A  = "AA2A", B = "AA2B", list(NA)),
    B = list(A = "ABA", B = "ABB", list(), list(NULL))
  ),
  B = list(
    A = list(A = "BAA", B = "BAB", NULL),
    B = list(A = "BBA", B = "BBB", list(NA)),
    B = list(A = "BB2A", B = "BB2B", list())
  ),
  C = list(
    A = list(1:10, NULL, NULL),
    B = list(letters, NULL, NULL),
    C = list("CAA", "CAA", "CAA")
  )
)

y <- list(
  A = list(
    A = data.frame(letters, LETTERS, 1:26),
    A = list(A  = "AA2A", B = "AA2B", list(NA)),
    B = list(A = "ABA", B = "ABB", list(), list(NULL))
  ),
  B = list(
    A = list(A = "BAA", B = "BAB", NULL),
    B = list(A = "BBA", B = "BBB", list(NA)),
    B = list(A = "BB2A", B = "BB2B", list())
  ),
  C = list(
    A = list(1:10, NULL, NULL),
    B = list(letters, NULL, NULL),
    C = list("CAA", "CAA", "CAA")
  )
)


expect_equal(
  basefun3(x),
  cast_hier2dim(x)
)


expect_equal(
  basefun3(x, FALSE),
  cast_hier2dim(x, FALSE)
)

expect_equal(
  basefun2(y),
  cast_hier2dim(y)
)


expect_equal(
  basefun2(y, FALSE),
  cast_hier2dim(y, FALSE)
)


expect_equal(
  basefun3(y),
  cast_hier2dim(y, recurse_classed = TRUE)
)

expect_equal(
  basefun3(y, FALSE),
  cast_hier2dim(y, FALSE, recurse_classed = TRUE)
)
enumerate <- enumerate + 6L



# depth limit test ====
x <- .rcpp_allocate_nestedlist(rep(2, 20), 1)
x2 <- cast_hier2dim(x)
expect_equal(
  ndim(x2),
  16L
)
expect_true(
  is.list(x2[[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]][[1]])
)

x <- .rcpp_allocate_nestedlist(rep(2, 20), 1)
x2 <- cast_hier2dim(x, maxdepth = 4L)
expect_equal(
  ndim(x2),
  4L
)
expect_true(
  is.list(x2[[1,1,1,1]][[rep(1, 15)]])
)

enumerate <- enumerate + 4L



# errors ====

expect_error(
  cast_hier2dim(c(x, data.frame(letters, LETTERS))),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  cast_hier2dim(rep(list(NULL), 10)),
  pattern = "not all surface elements have valid nested elements"
)
expect_error(
  cast_hier2dim(list()),
  pattern = "cannot cast zero-length list"
)
expect_error(
  cast_hier2dim(1:10),
  pattern = "`x` must be a list"
)
expect_error(
  cast_hier2dim(x, NA),
  pattern = "`in2out` must be `TRUE` or `FALSE`"
)
expect_error(
  cast_hier2dim(x, recurse_classed = NA),
  pattern = "`recurse_classed` must be `TRUE` or `FALSE`"
)
expect_error(
  hier2dim(matrix(as.list(1:10))),
  pattern = "`x` already has dimensions"
)

expect_error(
  cast_hier2dim(as.list(1:10), maxdepth = NA),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  cast_hier2dim(as.list(1:10), maxdepth = NA_integer_),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)
expect_error(
  cast_hier2dim(as.list(1:10), maxdepth = 1:10),
  pattern = "`maxdepth` must be a single integer between 2 and 16"
)

enumerate <- enumerate + 10L

