
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.rcpp_clone <- broadcast:::.rcpp_clone

coercionfuns <- list(
  as.logical,
  as.integer,
  as.double,
  as.complex,
  as.character,
  as.raw,
  as.list
)

# return unchanged ====
for(iType in seq_along(coercionfuns)) {
  for(iLen in 1:10) {
    x <- coercionfuns[[iType]](1:iLen)
    expect_equal(
      undim(x),
      x
    ) |> errorfun()
    enumerate <- enumerate + 1L
  }
  
}

# no names ====
for(iType in seq_along(coercionfuns)) {
  x <- array(coercionfuns[[iType]](1:27), c(3,3,3))
  expected <- .rcpp_clone(x)
  dim(expected) <- NULL
  expect_equal(
    undim(x),
    expected
  ) |> errorfun()
  
  enumerate <- enumerate + 1L
  
}


# arrays with only flat names ====
for(iType in seq_along(coercionfuns)) {
  for(iNdim in 1:3) {
    x.dim <- sample(1:10, iNdim, TRUE)
    iLen <- prod(x.dim)
    x <- array(coercionfuns[[iType]](1:iLen), x.dim)
    names(x) <- sample(letters, iLen, TRUE)
    
    expected <- x
    dim(expected) <- NULL
    names(expected) <- names(x)
    
    expect_equal(
      undim(x),
      expected
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
  }
}


# directional vectors with names ====
for(iType in seq_along(coercionfuns)) {
  for(iDir in 1:3) {
    for(iNdim in iDir:(iDir+3)) {
      for(iLen in seq(1, 20, 2)) {
        x.dim <- rep(1L, iNdim)
        x.dim[iDir] <- iLen
        x <- array(coercionfuns[[iType]](1:iLen), x.dim)
        dimnames(x) <- rep(list(NULL), iNdim)
        dimnames(x)[[iDir]] <- sample(letters, iLen)
        
        expected <- x
        dim(expected) <- NULL
        names(expected) <- dimnames(x)[[iDir]]
        
        expect_equal(
          undim(x),
          expected
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
      }
    }
  }
}


# broadcaster ====
x <- 1:10
broadcaster(x) <- TRUE
expect_equal(
  vector2array(x, 1, 2) |> broadcaster(),
  broadcaster(x)
)

broadcaster(x) <- FALSE
expect_equal(
  vector2array(x, 1, 2) |> broadcaster(),
  broadcaster(x)
)

enumerate <- enumerate + 2L


# errors ====
expect_error(
  undim(~ foo),
  pattern = "`x` must be atomic or a list"
)
enumerate <- enumerate + 1L


