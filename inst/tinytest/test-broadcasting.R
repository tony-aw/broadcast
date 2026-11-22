# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
errorfun <- broadcast:::.test_errorfun


# 2v  - aligned vectors ====
x <- as.list(1:10)
y <- as.list(1:10)
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)),
  mapply(\(x, y) list(x, y), x, y, SIMPLIFY = FALSE)
)
enumerate <- enumerate + 1L


# 2v - orthogonal vectors ====
x <- as_list(as.array(1:10))
y <- as_list(array(1:10, c(1, 10)))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), rep_dim(x, c(10, 10)), rep_dim(y, c(10, 10)), SIMPLIFY = FALSE)
)

x <- as_list(array(1:10, c(1, 10)))
y <- as_list(as.array(1:10))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), rep_dim(x, c(10, 10)), rep_dim(y, c(10, 10)), SIMPLIFY = FALSE)
)

enumerate <- enumerate + 2L


# 1v - big2vector ====
x <- as_list(matrix(1:100, 10, 10))
y <- as_list(as.array(1:10))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), x, rep_dim(y, c(10,10)), SIMPLIFY = FALSE)
)

x <- as_list(as.array(1:10))
y <- as_list(matrix(1:100, 10, 10))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), rep_dim(x, c(10,10)), y, SIMPLIFY = FALSE)
)

enumerate <- enumerate + 2L



# 1v - sandwiched vectors ====
x <- as_list(array(1:100, c(10, 1, 10)))
y <- as_list(as.array(1:10))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), x, rep_dim(y, c(10,10)), SIMPLIFY = FALSE)
)

x <- as_list(as.array(1:10))
y <- as_list(array(1:100, c(10, 1, 10)))
expect_equal(
  bc.list(x, y, \(x, y) list(x, y)) |> undim(),
  mapply(\(x, y) list(x, y), rep_dim(x, c(10,10)), y, SIMPLIFY = FALSE)
)

enumerate <- enumerate + 2L


# orthogonal ====
n <- function()sample(c(2:5), 1L)
for(i in 2:8) {
  print(i)
  
  # start x:
  x.dim <- rep(c(n(), 1L), i)[1:i]
  y.dim <- rep(c(1L, n()), i)[1:i]
  x <- array(as.list(1:prod(x.dim)), x.dim)
  y <- array(as.list(1:prod(y.dim)), y.dim)
  tdim <- pmax(x.dim, y.dim)
  
  expect_equal(
    bc.list(x, y, \(x, y) list(x, y)) |> undim(),
    mapply(\(x, y) list(x, y), rep_dim(x, tdim), rep_dim(y, tdim), SIMPLIFY = FALSE)
  ) |> errorfun()
  
  
  # start y:
  x.dim <- rep(c(1L, n()), i)[1:i]
  y.dim <- rep(c(n(), 1L), i)[1:i]
  x <- array(as.list(1:prod(x.dim)), x.dim)
  y <- array(as.list(1:prod(y.dim)), y.dim)
  tdim <- pmax(x.dim, y.dim)
  
  expect_equal(
    bc.list(x, y, \(x, y) list(x, y)) |> undim(),
    mapply(\(x, y) list(x, y), rep_dim(x, tdim), rep_dim(y, tdim), SIMPLIFY = FALSE)
  ) |> errorfun()
  
  
  enumerate <- enumerate + 2L
  
}



# orthogonal with one shared dim ====
n <- function()sample(c(2:5), 1L)
for(i in 2:8) {
  print(i)
  
  # start x:
  x.dim <- rep(c(n(), 1L), i)[1:i]
  y.dim <- rep(c(1L, n()), i)[1:i]
  ind <- sample(1:i, sample(1:(i-2), 1))
  x.dim[ind] <- y.dim[ind] <- sample(2:5, 1L)
  
  x <- array(as.list(1:prod(x.dim)), x.dim)
  y <- array(as.list(1:prod(y.dim)), y.dim)
  tdim <- pmax(x.dim, y.dim)
  
  expect_equal(
    bc.list(x, y, \(x, y) list(x, y)) |> undim(),
    mapply(\(x, y) list(x, y), rep_dim(x, tdim), rep_dim(y, tdim), SIMPLIFY = FALSE)
  ) |> errorfun()
  
  
  # start y:
  x.dim <- rep(c(1L, n()), i)[1:i]
  y.dim <- rep(c(n(), 1L), i)[1:i]
  x <- array(as.list(1:prod(x.dim)), x.dim)
  y <- array(as.list(1:prod(y.dim)), y.dim)
  tdim <- pmax(x.dim, y.dim)
  
  expect_equal(
    bc.list(x, y, \(x, y) list(x, y)) |> undim(),
    mapply(\(x, y) list(x, y), rep_dim(x, tdim), rep_dim(y, tdim), SIMPLIFY = FALSE)
  ) |> errorfun()
  
  
  enumerate <- enumerate + 2L
  
}
