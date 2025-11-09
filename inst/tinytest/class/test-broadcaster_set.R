
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

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


# broadcaster ====

for(iData in seq_along(x.data)) {
  for(iClass in list(NULL, "mutatomic")) {
    for(iDim in list(NULL, c(10, 10))) {
      for(iAttr in c(TRUE, FALSE)) {
        
        # make input:
        x <- x.data[[iData]]
        class(x) <- iClass
        dim(x) <- iDim
        if(iAttr) {
          attr(x, "test") <- c("hello", "goodbye")
        }
        
        # initially false:
        
        expect_false(
          broadcaster(x)
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
        # still false:
        y <- x
        broadcaster(y) <- FALSE
        expect_equal(
          x, y
        ) |> errorfun() 
        expect_false(
          broadcaster(x)
        ) |> errorfun() 
        expect_false(
          broadcaster(y)
        ) |> errorfun() 
        enumerate <- enumerate + 3L
        
        # becomes true:
        broadcaster(x) <- TRUE
        expect_true(
          broadcaster(x)
        ) |> errorfun()
        
        # still true:
        y <- x
        broadcaster(y) <- TRUE
        expect_equal(
          x, y
        ) |> errorfun() 
        expect_true(
          broadcaster(x)
        ) |> errorfun() 
        expect_true(
          broadcaster(y)
        ) |> errorfun() 
        enumerate <- enumerate + 3L
        
        # false again:
        broadcaster(x) <- FALSE
        expect_false(
          broadcaster(x)
        ) |> errorfun() 
        
        enumerate <- enumerate + 1L
        
      }
    }
  }
}



x <- factor(letters)
class(x) <- c("broadcaster", "factor")
attr(x, 'typeof') <- typeof(x)
expect_false(
  broadcaster(x)
)

enumerate <- enumerate + 1L


# couldb.broadcaster ====
for(i in seq_along(x.data)) {
  x <- x.data[[i]]
  expect_true(
    broadcast:::.couldb.broadcaster(x)
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}

