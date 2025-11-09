
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

test_make_dims <- function(n) {
  
  # make dimensions that are randomly of size 4 or 16:
  out <- lapply(1:n, \(n)sample(c(4, 16), 1)) |> unlist()
  
  # check if the dimensions produce a too large object.
  # If so, replace one >1L dimension with 1L
  if(prod(out) > 5000L) {
    ind <- which(out > 1L)[1L]
    out[ind] <- 1L
  }
  return(out)
}


test_make_dimnames <- function(x.dim) {
  out <- lapply(x.dim, \(n) sample(letters, n, TRUE))
  if(length(out) > 1) {
    out[sample(1:length(out), 1)] <- list(NULL)
  }
  return(out)
}


datagens <- list(
  \() as.raw(sample(1:10)), # R really does not work well with raw types
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)



for(iNdims in c(1:5)) {
  for(iNamed in c(TRUE, FALSE)) {
    for(iMargin in c(1, iNdims)) {
      for(iType in seq_along(datagens)) {
        for(iFill in c(TRUE, FALSE)) {
          for(iSample in 1:5) {
            
            x <- array(datagens[[iType]](), test_make_dims(iNdims))
            if(iNamed) {
              dimnames(x) <- test_make_dimnames(dim(x))
            }
            
            if(iFill) {
              grp <- as.factor(sample(1:(dim(x)[iMargin] - 1L), dim(x)[iMargin], TRUE))
            }
            else {
              grp <- as.factor(sample(rep_len(1:sqrt(dim(x)[iMargin]), dim(x)[iMargin])))
            }
            
            if(is.atomic(x)) {
              fillvalue <- x[sample(1:length(x), 1L)]
            }
            if(is.recursive(x)) {
              fillvalue <- x[sample(1:length(x), 1L)]
            }
            
            if(nlevels(grp) >= 2L) {
              
              
              expect_silent(
                acast(x, iMargin, grp, iFill, fillvalue)
              ) |> errorfun()
              
              check_dimnames <- dimnames(acast(x, iMargin, grp, iFill, fillvalue))
              expect_true(
                !is.null(check_dimnames[length(check_dimnames)])
              ) |> errorfun()
              
              enumerate <- enumerate + 2L
              
              if(iNamed && iNdims >= 2L) {
                out <- acast(x, iMargin, grp, iFill, fillvalue)
                expect_equal(
                  dimnames(x)[-iMargin],
                  dimnames(out)[c(-iMargin, -ndim(out))]
                ) |> errorfun()
                enumerate <- enumerate + 1L
              }
              
              
            }
          }
        }
        
      }
    }
  }
}
