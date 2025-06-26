
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

as.someclass <- function(x) {
  class(x) <- c("someclass", class(x))
  return(x)
}

ab <- broadcast:::.as.broadcaster

# check if broadcaster works even with overwriting class ====

x <- 1:10
y <- array(1:10, c(1, 10))
broadcaster(x) <- TRUE
broadcaster(y) <- TRUE

expect_equivalent(
  unclass(as.someclass(x) + as.someclass(y)),
  as.someclass(x + y) |> unclass()
)


# check if the broadcaster class allows other classes to overwrite method ====

`+.someclass` <- function(x, y) {
  unclass(x) + unclass(y)
}

expect_equal(
  ab(x) + ab(y),
  broadcast:::`+.broadcaster`(x, y)
)

# check that bc_chain wins even with overwriting method ====


`+.someclass` <- function(x, y) {
  unclass(x) + unclass(y)
}

expect_equal(
  unclass(x) + unclass(y),
  as.someclass(x) + as.someclass(y)
)

rm(`+.someclass`)




enumerate <- enumerate + 3L
