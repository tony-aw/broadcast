# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}



# check special symbols ====
`TRUE` <- sample(1:10)
expect_silent(
  `TRUE` %orientbc<-% 2L
)
expect_true(
  broadcaster(`TRUE`)
)
expect_equal(
  dim(`TRUE`),
  c(1L, 10L)
)
enumerate <- enumerate + 3L


# check overwriting function internal variables ====
# new.dim and new.dimnames are the variable names used inside `%orientbc<-%`
# here we test if variables with the same names in the caller environment interfere at all
new.dim <- c(1L, 1L, 10L)
new.dimnames <- list(NULL, NULL, month.name[1:10])
x <- setNames(sample(1:10), month.abb[1:10])
x %orientbc<-% 2L

expect_equal(
  dim(x), c(1L, 10L)
)
expect_equal(
  dimnames(x), list(NULL, month.abb[1:10])
)
enumerate <- enumerate + 2L


# check as returning function ====
tempfun <- function(x, orient, ndim) {
  x %orientbc<-% c(orient, ndim)
  return(x)
}

x <- sample(1:10)
for(iOrient in 1:16) {
  for(iNdim in seq(iOrient, 16L)) {
    for(iBc in c(TRUE, FALSE)) {
      for(iNamed in c(TRUE, FALSE)) {
        
        if(iNamed) names(x) <- sample(month.abb, length(x))
        broadcaster(x) <- iBc
        
        out <- tempfun(x, iOrient, iNdim)
        
        expect_equal(
            broadcaster(out), TRUE
        ) |> errorfun()
        
        expect_equal(
          dimnames(out)[[iOrient]], names(x)
        ) |> errorfun()
        
        expected.dim <- rep(1L, iNdim)
        expected.dim[iOrient] <- length(x)
        expect_equal(
          dim(out), expected.dim
        ) |> errorfun()
        
        expect_equal(
          as.vector(x),
          as.vector(out)
        ) |> errorfun()
        
        enumerate <- enumerate + 4L
        
      }
    }
  }
}


# check as passing function ====
tempfun <- function(x, orient, ndim) {
  eval(substitute(x %orientbc<-% c(orient, ndim)), envir = parent.frame())
}

x <- sample(1:10)
for(iOrient in 1:16) {
  for(iNdim in seq(iOrient, 16L)) {
    for(iBc in c(TRUE, FALSE)) {
      for(iNamed in c(TRUE, FALSE)) {
        
        if(iNamed) names(x) <- sample(month.abb, length(x))
        broadcaster(x) <- iBc
        
        out <- x
        tempfun(out, iOrient, iNdim)
        
        expect_equal(
          broadcaster(out), TRUE
        ) |> errorfun()
        
        expect_equal(
          dimnames(out)[[iOrient]], names(x)
        ) |> errorfun()
        
        expected.dim <- rep(1L, iNdim)
        expected.dim[iOrient] <- length(x)
        expect_equal(
          dim(out), expected.dim
        ) |> errorfun()
        
        expect_equal(
          as.vector(x),
          as.vector(out)
        ) |> errorfun()
        
        enumerate <- enumerate + 4L
        
      }
    }
  }
}


# check no unnecessary copies ====
.rcpp_address <- broadcast:::.rcpp_address
y <- rnorm(1e6)
y %orientbc<-% c(2L, 2L)
address1 <- .rcpp_address(y) # get address after first change
y %orientbc<-% c(3L, 3L)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)
enumerate <- enumerate + 1L


# check large dimnames are not secretly implemented symbolically and explode memory ====
nms <- rep_len(c(letters, LETTERS, month.abb, month.name), 1e7)
y <- setNames(rnorm(1e7), nms)

expect_silent(
  y %orientbc<-% 2L
)

expect_equal(
  dimnames(y)[[2L]], nms
)
enumerate <- enumerate + 2L


# idiot-proof tests ====

x <- setNames(1:10, letters[1:10])
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
  x %orientbc<-% 2L
)
expect_true(
  broadcast::broadcaster(x)
)
expect_equal(
  base::dim(x), c(1L, base::length(x))
)

rm(
  broadcaster, `broadcaster<-`, .couldb.broadcaster,
  class, `class<-`, oldClass, `oldClass<-`,
  dim, `dim<-`, dimnames, `dimnames<-`,
  `<-`
)

enumerate <- enumerate + 3L


