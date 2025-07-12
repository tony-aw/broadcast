
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_address <- broadcast:::.rcpp_address

# check consistency between dim2hier and hier2dim ====

for(i in 1:10) {
  x.dim <- sample(1:10, sample(2:4, 1))
  x <- array(
    sample(c(as.list(1:10), list(NULL))),
    x.dim
  )
  
  expect_equal(
    cast_dim2hier(x) |> cast_hier2dim(),
    x
  ) |> errorfun()
  enumerate<- enumerate + 1L
}


# no unnecessary copies tests ====

x <- array(as.list(1:60), 3:5)

for(i in 1:3) {
  for(j in 1:4) {
    for(k in 1:5) {
      
      out <- cast_dim2hier(x)
      expect_equal(
        x[[i, j, k]],
        out[[c(k, j, i)]]
      ) |> errorfun()
      expect_equal(
        .rcpp_address(x[[i, j, k]]),
        .rcpp_address(out[[c(k, j, i)]])
      ) |> errorfun()
      
      out <- cast_dim2hier(x, in2out = FALSE)
      expect_equal(
        x[[i, j, k]],
        out[[c(i, j, k)]]
      ) |> errorfun()
      expect_equal(
        .rcpp_address(x[[i, j, k]]),
        .rcpp_address(out[[c(i, j, k)]])
      ) |> errorfun()
      
      enumerate <- enumerate + 4L
      
    }
  }
}


for(i in 1:3) {
  for(j in 1:4) {
    for(k in 1:5) {
      
      ind <- paste("[", i, ", ", j, ", ", k, "]", sep = "")
      
      out <- cast_dim2flat(x)
      expect_equal(
        x[[i, j, k]],
        out[[ind]]
      ) |> errorfun()
      expect_equal(
        .rcpp_address(x[[i, j, k]]),
        .rcpp_address(out[[ind]])
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
      
    }
  }
}



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

hier2dim(x)

for(i in 1:3) {
  for(j in 1:3) {
    for(k in 1:3) {
      
      out <- cast_hier2dim(x)
      expect_equal(
        x[[c(i, j, k)]],
        out[[k, j, i]]
      ) |> errorfun()
      expect_equal(
        .rcpp_address(x[[c(i, j, k)]]),
        .rcpp_address(out[[k, j, i]])
      ) |> errorfun()
      
      out <- cast_hier2dim(x, in2out = FALSE)
      expect_equal(
        x[[c(i, j, k)]],
        out[[i, j, k]]
      ) |> errorfun()
      expect_equal(
        .rcpp_address(x[[c(i, j, k)]]),
        .rcpp_address(out[[i, j, k]])
      ) |> errorfun()
      
      enumerate <- enumerate + 4L
      
    }
  }
}


# Long ATOMIC vectors WITHIN lists allowed ====

x <- list(
  list(1:(2^52 - 1)),
  list(1:(2^52 - 1)),
  list(1:(2^52 - 1)),
  list(1:(2^52 - 1))
)

expect_silent(
  dropnests(x)
)
expect_silent(
  depth_range(x)
)

expect_silent(
  hier2dim(x)
)
expect_silent(
  cast_hier2dim(x)
)

dim(x) <- c(2,2)
expect_silent(
  cast_dim2hier(x)
)
expect_silent(
  cast_dim2flat(x)
)

enumerate <- enumerate + 6L


