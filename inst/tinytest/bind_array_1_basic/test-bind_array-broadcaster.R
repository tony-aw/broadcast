
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
unattr <- function(x) {
  out <- x
  attributes(out) <- NULL
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  return(out)
}


# one of the objects is broadcaster ====

x <- array(1:27, c(3,3,3))
y <- array(1:27, c(3,3,3))
z <- array(1:27, c(3,3,3))
input <- list(x, y, z)
expect_false(
  bind_array(input, 2L) |> broadcaster()
)

bcr(y) <- TRUE
input2 <- list(x, y, z)
expect_true(
  bind_array(input2, 2L) |> broadcaster()
)
expect_equal(
  bind_array(input2, 2L) |> unclass(),
  bind_array(input, 2L)
)
enumerate <- enumerate + 3L

# 
# # one of the objects is mutatomic (from squarebrackets package) ====
# x <- array(1:27, c(3,3,3))
# y <- array(1:27, c(3,3,3))
# z <- array(1:27, c(3,3,3))
# input <- list(x, y, z)
# class(y) <- "mutatomic" 
# input2 <- list(x, y, z)
# expect_equal(
#   bind_array(input2, 2L) |> class(),
#   "mutatomic"
# )
# expect_equal(
#   bind_array(input2, 2L) |> unattr(),
#   bind_array(input, 2L)
# )
# enumerate <- enumerate + 2L
# 
# 
# 
# # one of the objects is broadcaster, another object is mutatomic ====
# x <- array(1:27, c(3,3,3))
# y <- array(1:27, c(3,3,3))
# z <- array(1:27, c(3,3,3))
# input <- list(x, y, z, x)
# class(y) <- "mutatomic"
# class(z) <- "broadcaster"
# input2 <- list(x, y, z, x)
# expect_equal(
#   bind_array(input2, 2L) |> class(),
#   c("mutatomic", "broadcaster")
# )
# expect_equal(
#   bind_array(input2, 2L) |> unattr(),
#   bind_array(input, 2L)
# )
# enumerate <- enumerate + 2L
# 
# 
# # one of the objects is both broadcaster AND mutatomic ====
# x <- array(1:27, c(3,3,3))
# y <- array(1:27, c(3,3,3))
# z <- array(1:27, c(3,3,3))
# input <- list(x, y, z)
# class(y) <- "mutatomic"
# broadcaster(y) <- TRUE
# input2 <- list(x, y, z)
# expect_equal(
#   bind_array(input2, 2L) |> class(),
#   c("mutatomic", "broadcaster")
# )
# expect_equal(
#   bind_array(input2, 2L) |> unattr(),
#   bind_array(input, 2L)
# )
# enumerate <- enumerate + 2L
# 
