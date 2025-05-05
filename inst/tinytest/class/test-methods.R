
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# sub-setting ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
expect_equal(
  broadcast:::.as.broadcaster(x[1:2, ,drop = FALSE]),
  y[1:2, , drop = FALSE]
 
)
expect_equal(
  broadcast:::.as.broadcaster(x[,1:2 ,drop = FALSE]),
  y[,1:2 , drop = FALSE]
  
)
expect_equal(
  broadcast:::.as.broadcaster(x[2:3]),
  y[2:3]
)

enumerate <- enumerate + 3L



#  replacement ====
x <- matrix(1:20, ncol = 4)
y <- broadcast:::.as.broadcaster(x)
x[1] <- -1
y[1] <- -1
expect_equal(
  broadcast:::.as.broadcaster(x),
  y
)
enumerate <- enumerate + 3L



# Concatenation ====
x <- broadcast:::.as.broadcaster(1:10)
y <- broadcast:::.as.broadcaster(11:20)
expect_equal(
  c(x, y),
  broadcast:::.as.broadcaster(1:20)
)
enumerate <- enumerate + 1L


# as.* ====

x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
funs <- list(
  as.logical,
  as.integer,
  as.double,
  as.complex,
  as.character,
  as.raw,
  as.list
)

for(i in seq_along(x.data)) {
  for(j in seq_along(funs)) {
    
    # prep:
    x <- x.data[[i]]
    y <- x
    z <- x
    broadcaster(y) <- TRUE
    y <- funs[[j]](y)
    z <- funs[[j]](z)
    broadcaster(z) <- TRUE
    
    # tests:
    expect_equal(
      y, z
    ) |> errorfun()
    expect_true(
      broadcaster(y)
    ) |> errorfun()
    expect_true(
      broadcaster(z)
    ) |> errorfun()
    expect_false(
      broadcaster(x)
    ) |> errorfun()
    
    enumerate <- enumerate + 4L
    
  }
  
}


