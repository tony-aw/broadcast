

# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_allocate_nestedlist <- broadcast:::.rcpp_allocate_nestedlist

element.exists <- function(var, element) {
  # from https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists/7719860
  tryCatch({
    if(length(var[[element]]) > -1)
      return(T)
  }, error = function(e) {
    return(F)
  })
}

basefun3 <- function(x, in2out = TRUE, padding = list(NULL)) {
  dims <- unname(broadcast:::.hiercast_dims(x, 3L, in2out, FALSE, sys.call()))
  if(in2out) {
    out <- array(padding, dims)
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        for(k in 1:dims[3]) {
          if(element.exists(x, c(k, j, i))) {
            temp <- x[[c(k, j, i)]]
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
  }
  else {
    out <- array(padding, dims)
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        for(k in 1:dims[3]) {
          if(element.exists(x, c(i, j, k))) {
            temp <- x[[c(i, j, k)]]
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
  }
  return(out)
}

basefun2 <- function(x, in2out = TRUE, padding = list(NULL)) {
  dims <- unname(broadcast:::.hiercast_dims(x, 2L, in2out, FALSE, sys.call()))
  if(in2out) {
    out <- array(padding, dims)
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        if(element.exists(x, c(j, i))) {
          temp <- x[[c(j, i)]]
          if(is.null(temp)) {
            out[i, j] <- list(NULL)
          }
          else {
            out[[i, j]] <- temp
          }
        }
      }
    }
  }
  else {
    out <- array(padding, dims)
    for(i in 1:dims[1]) {
      for(j in 1:dims[2]) {
        if(element.exists(x, c(i, j))) {
          temp <- x[[c(i, j)]]
          if(is.null(temp)) {
            out[i, j] <- list(NULL)
          }
          else {
            out[[i, j]] <- temp
          }
        }
      }
    }
  }
  return(out)
}


mat <- matrix(NA, 10, 10, dimnames = list(letters[1:10], LETTERS[1:10]))
attr(mat, "test") <- "test"

x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB", list(NULL)),
    A = list(A  = "AA2A", B = "AA2B", mat),
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
    B = list(A = "BBA", B = "BBB", mat),
    B = list(A = "BB2A", B = "BB2B", list())
  ),
  C = list(
    A = list(1:10, NULL, NULL),
    B = list(letters, NULL, NULL),
    C = list("CAA", "CAA", "CAA")
  )
)


# default functionality tests ====

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
  cast_hier2dim(y, recurse_all = TRUE)
)

expect_equal(
  basefun3(y, FALSE),
  cast_hier2dim(y, FALSE, recurse_all = TRUE)
)
enumerate <- enumerate + 6L



# non-default functionality tests ====

expect_equal(
  basefun3(x, padding = list(~ "this is padding")),
  cast_hier2dim(x, padding = list(~ "this is padding"))
)


expect_equal(
  basefun3(x, FALSE, padding = list(~ "this is padding")),
  cast_hier2dim(x, FALSE, padding = list(~ "this is padding"))
)

expect_equal(
  basefun2(y, padding = list(~ "this is padding")),
  cast_hier2dim(y, padding = list(~ "this is padding"))
)


expect_equal(
  basefun2(y, FALSE, padding = list(~ "this is padding")),
  cast_hier2dim(y, FALSE, padding = list(~ "this is padding"))
)


expect_equal(
  basefun3(y, padding = list(~ "this is padding")),
  cast_hier2dim(y, recurse_all = TRUE, padding = list(~ "this is padding"))
)

expect_equal(
  basefun3(y, FALSE, padding = list(~ "this is padding")),
  cast_hier2dim(y, FALSE, recurse_all = TRUE, padding = list(~ "this is padding"))
)
enumerate <- enumerate + 6L




# errors ====

expect_error(
  cast_hier2dim(x, padding = NA),
  pattern = "`padding` must be a list of length 1"
)
expect_error(
  cast_hier2dim(x, padding = as.list(1:10)),
  pattern = "`padding` must be a list of length 1"
)

enumerate <- enumerate + 2L

