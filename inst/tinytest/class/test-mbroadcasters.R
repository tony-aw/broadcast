
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


# main tests ====

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
        mbroadcasters("y", FALSE)
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
        mbroadcasters("x", TRUE)
        expect_true(
          broadcaster(x)
        ) |> errorfun()
        
        # still true:
        y <- x
        mbroadcasters("y", TRUE)
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
        mbroadcasters("x", FALSE)
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




# testing ignoring mechanism - value = TRUE ====
x <- factor(letters) # this will be ignored
y <- sample(1:10) # this will become a broadcaster
z <- array(1:27, c(3,3,3)) # this will also be ignored
class(z) <- "broadcaster"
expect_silent(
  mbroadcasters(c("x", "y", "z"), TRUE)
)

x <- factor(letters) # this will be ignored
y <- sample(1:10) # this will become a broadcaster
z <- array(1:27, c(3,3,3)) # this will also be ignored
class(z) <- "broadcaster"
mbroadcasters(c("x", "y", "z"), TRUE)


expect_false(
  broadcaster(x)
)
expect_true(
  broadcaster(y)
)
expect_equal(
  class(z),
  "broadcaster"
)

enumerate <- enumerate + 4L




# testing ignoring mechanism - value = FALSE ====
x <- factor(letters) # this will be ignored
y <- sample(1:10) # this will be ignored
z <- array(1:27, c(3,3,3)) # this will become a regular array
class(z) <- "broadcaster"
expect_silent(
  mbroadcasters(c("x", "y", "z"), FALSE)
)

x <- factor(letters) # this will be ignored
y <- sample(1:10) # this will be ignored
class(y) <- "foo"
z <- array(1:27, c(3,3,3)) # this will become a regular array
class(z) <- "broadcaster"
mbroadcasters(c("x", "y", "z"), FALSE)


expect_false(
  broadcaster(x)
)
expect_equal(
  class(y),
  "foo"
)
expect_false(
  broadcaster(z)
)

enumerate <- enumerate + 4L


# sub-environment usage ====
env <- new.env()
env$x <- 1:10
mbroadcasters("x", TRUE, env)
expect_true(
  broadcaster(env$x)
)
mbroadcasters("x", FALSE, env)
expect_false(
  broadcaster(env$x)
)
enumerate <- enumerate + 2L


# errors ====
expect_error(
  mbroadcasters(~ foo, TRUE),
  pattern = "`nms` must be a character vector"
)

expect_error(
  mbroadcasters(letters, NA),
  pattern = "`value` must be `TRUE` or `FALSE`"
)

expect_error(
  mbroadcasters(letters, TRUE, ~ foo),
  "`env` must be an environment or `NULL`"
)


# tes protected names ====
`TRUE` <- 1:10
`1` <- 1:10
expect_error(
  mbroadcasters("TRUE", TRUE)
)
expect_error(
  mbroadcasters("1", TRUE)
)
mbroadcasters("`TRUE`", TRUE)
expect_true(
  broadcaster(`TRUE`)
)
mbroadcasters("`1`", TRUE)
expect_true(
  broadcaster(`1`)
)

enumerate <- enumerate + 4L

