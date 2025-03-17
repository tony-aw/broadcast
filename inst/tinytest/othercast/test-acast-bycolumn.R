
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
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

datagens <- list(
  #  \() as.raw(sample(1:10)), # R really does not work well with raw types
  \() sample(c(TRUE, FALSE, NA), 10L, TRUE),
  \() sample(c(-10L:10L, NA_integer_)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)),
  \() sample(c(rnorm(10), NA, NaN, Inf, -Inf)) + sample(c(rnorm(10), NA, NaN, Inf, -Inf)) * -1i,
  \() sample(c(letters, NA)),
  \() sample(list(letters, month.abb, 1:10))
)


margin <- 2L

# 2d ====

for(iType in seq_along(datagens)) {
  for(iFill in c(TRUE, FALSE)) {
    for(iSample in 1:5) {

      if(iFill) {
        x <- array(datagens[[iType]](), test_make_dims(2L))
        grp <- as.factor(sample(1:(ncol(x)-1L), ncol(x), TRUE))
        if(nlevels(grp) >= 2L) {
          levels(grp) <- letters[sample(1:nlevels(grp))]
          
          maxfreq <- max(tabulate(unclass(grp)))
          out.dim <- c(dim(x), length(unique(grp)))
          out.dim[margin] <- maxfreq
          if(is.atomic(x)) {
            fillvalue <- NA
          }
          if(is.recursive(x)) {
            fillvalue <- list(NULL)
          }
          
          out <- array(fillvalue, out.dim)
          
          for(k in 1:nlevels(grp)) {
            extract <- x[, grp == levels(grp)[k], drop=FALSE]
            out[1:nrow(extract), 1:ncol(extract), k] <- extract
          }
          
          expect_equivalent(
            acast(x, margin, grp, TRUE),
            out
          ) |> errorfun()
        }
        
      }
        
      else if(!iFill) {
        x <- array(datagens[[iType]](), test_make_dims(2L))
        grp <- as.factor(sample(rep_len(1:sqrt(ncol(x)), ncol(x))))
        levels(grp) <- letters[sample(1:nlevels(grp))]
        
        maxfreq <- max(tabulate(unclass(grp)))
        out.dim <- c(dim(x), length(unique(grp)))
        out.dim[margin] <- maxfreq
        fillvalue <- datagens[[iType]]()[1L]
        out <- array(fillvalue, out.dim)
        
        for(k in 1:nlevels(grp)) {
          extract <- x[, grp == levels(grp)[k] , drop = FALSE]
          out[, , k] <- extract
        }
        
        expect_equivalent(
          acast(x, margin, grp),
          out
        ) |> errorfun()
        
      }
        enumerate <- enumerate + 1L
        
      
    }
  }
 
}


# 1d ====

for(iType in seq_along(datagens)) {
  for(iFill in c(TRUE, FALSE)) {
    for(iSample in 1:5) {
      
      if(iFill) {
        x <- array(datagens[[iType]](), c(1L, test_make_dims(1L)))
        grp <- as.factor(sample(1:(ncol(x)-1L), ncol(x), TRUE))
        if(nlevels(grp) >= 2L) {
          levels(grp) <- letters[sample(1:nlevels(grp))]
          
          maxfreq <- max(tabulate(unclass(grp)))
          out.dim <- c(dim(x), length(unique(grp)))
          out.dim[margin] <- maxfreq
          if(is.atomic(x)) {
            fillvalue <- NA
          }
          if(is.recursive(x)) {
            fillvalue <- list(NULL)
          }
          
          out <- array(fillvalue, out.dim)
          
          for(k in 1:nlevels(grp)) {
            extract <- x[, grp == levels(grp)[k], drop=FALSE]
            out[, 1:ncol(extract), k] <- extract
          }
          
          expect_equivalent(
            acast(x, margin, grp, TRUE),
            out
          ) |> errorfun()
        }
        
      }
      
      else if(!iFill) {
        x <- array(datagens[[iType]](), c(1L, test_make_dims(1L)))
        grp <- as.factor(sample(rep_len(1:sqrt(ncol(x)), ncol(x))))
        levels(grp) <- letters[sample(1:nlevels(grp))]
        
        maxfreq <- max(tabulate(unclass(grp)))
        out.dim <- c(dim(x), length(unique(grp)))
        out.dim[margin] <- maxfreq
        fillvalue <- datagens[[iType]]()[1L]
        out <- array(fillvalue, out.dim)
        
        for(k in 1:nlevels(grp)) {
          extract <- x[, grp == levels(grp)[k], drop = FALSE]
          out[, , k] <- extract
        }
        
        expect_equivalent(
          acast(x, margin, grp),
          out
        ) |> errorfun()
        
      }
      enumerate <- enumerate + 1L
      
      
    }
  }
  
}

