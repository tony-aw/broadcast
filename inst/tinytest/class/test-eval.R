# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# check special symbols ====
`TRUE` <- 1:10
`1` <- 1:10
mbroadcasters("TRUE", TRUE)
expect_true(
  broadcaster(`TRUE`)
)
mbroadcasters("1", TRUE)
expect_true(
  broadcaster(`1`)
)

enumerate <- enumerate + 2L



# check overwriting function internal variables ====
# sym is the variable names used inside mbroadcasters()
# here we test if variables with the same names in the caller environment interfere at all
sym <- as.name("y")
y <- setNames(sample(1:10), month.abb[1:10])
x <- setNames(sample(1:10), month.abb[1:10])

mbroadcasters("x", TRUE)

expect_true(
  broadcaster(x)
)
expect_false(
  broadcaster(y)
)

y <- as.name("x")
sym <- setNames(sample(1:10), month.abb[1:10])
x <- setNames(sample(1:10), month.abb[1:10])

mbroadcasters("x", TRUE)

expect_true(
  broadcaster(x)
)
expect_false(
  broadcaster(sym)
)


enumerate <- enumerate + 4L


# check as returning function ====
tempfun <- function(x, value) {
  mbroadcasters("x", value)
  return(x)
}

x <- sample(1:10)
expect_false(
  broadcaster(x)
)
expect_true(
  broadcaster(tempfun(x, TRUE))
)
expect_false(
  broadcaster(x)
)
broadcaster(x) <- TRUE
expect_false(
  broadcaster(tempfun(x, FALSE))
)
expect_true(
  broadcaster(x)
)
enumerate <- enumerate + 4L


# check as passing function ====
tempfun <- function(nms, value) {
  mbroadcasters(nms, value, parent.frame())
}

x <- sample(1:10)
expect_false(
  broadcaster(x)
)
tempfun("x", TRUE)
expect_true(
  broadcaster(x)
)
tempfun("x", FALSE)
expect_false(
  broadcaster(x)
)
enumerate <- enumerate + 3L


# check no unnecessary copies ====
.rcpp_address <- broadcast:::.rcpp_address
y <- rnorm(1e6)
mbroadcasters("y", TRUE)
address1 <- .rcpp_address(y) # get address after first change
mbroadcasters("y", FALSE)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)
mbroadcasters("y", TRUE)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)
enumerate <- enumerate + 2L


# Idiot proof tests ====
x <- 1:10
broadcaster <- function(x) stop("you're an idiot")
.couldb.broadcaster <- function(x) stop("you're an idiot")
`broadcaster<-` <- function(x, value) stop("you're an idiot")
class <- function(x) stop("you're an idiot")
`class<-` <- function(x) stop("you're an idiot")
oldClass <- function(x) stop("you're an idiot")
`oldClass<-` <- function(x) stop("you're an idiot")
dim <- function(x) stop("you're an idiot")
dimnames <- function(x) stop("you're an idiot")
`dim<-` <- function(x) stop("you're an idiot")
`dimnames<-` <- function(x) stop("you're an idiot")
`<-` <- function(x, y) stop("you're an idiot")

expect_silent(
  mbroadcasters("x", TRUE)
)
expect_true(
  broadcast::broadcaster(x)
)
expect_silent(
  mbroadcasters("x", FALSE)
)
expect_false(
  broadcast::broadcaster(x)
)

rm(
  broadcaster, `broadcaster<-`, .couldb.broadcaster,
  class, `class<-`, oldClass, `oldClass<-`,
  dim, `dim<-`, dimnames, `dimnames<-`,
  `<-`
)

enumerate <- enumerate + 4L


