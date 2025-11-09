
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

source("source.R")

margin <- 1L


# 2d ====

for(iType in seq_along(datagens)) {
  for(iFill in c(TRUE, FALSE)) {
    for(iNamed in c(TRUE, FALSE)) {
      for(iSample in 1:5) {
        
        if(iFill) {
          x <- array(datagens[[iType]](), test_make_dims(2L))
          if(iNamed) dimnames(x) <- test_make_dimnames(dim(x))
          
          grp <- as.factor(sample(1:(nrow(x)-1L), nrow(x), TRUE))
          if(nlevels(grp) >= 2L) {
            levels(grp) <- letters[sample(1:nlevels(grp))]
            
            maxfreq <- max(tabulate(unclass(grp)))
            out.dim <- c(dim(x), length(unique(grp)))
            out.dim[margin] <- maxfreq
            
            fillvalue <- test_make_fillval(x)
            
            out.dimnames <- NULL
            if(iNamed) {
              out.dimnames <- rep(NULL, length(out.dim))
              out.dimnames[1:ndim(x)] <- dimnames(x)
              out.dimnames[margin] <- list(NULL)
              out.dimnames[[ndim(x) + 1]] <- levels(grp)
            }
            
            out <- array(fillvalue, out.dim, out.dimnames)
            
            for(k in 1:nlevels(grp)) {
              extract <- x[grp == levels(grp)[k],, drop=FALSE]
              out[1:nrow(extract), 1:ncol(extract), k] <- extract
            }
            
            if(iNamed) {
              expect_equal(
                acast(x, margin, grp, TRUE, fillvalue),
                out
              ) |> errorfun()
            }
            else {
              expect_equivalent(
                acast(x, margin, grp, TRUE, fillvalue),
                out
              ) |> errorfun()
            }
            
          }
          
        }
        
        else if(!iFill) {
          x <- array(datagens[[iType]](), test_make_dims(2L))
          if(iNamed) dimnames(x) <- test_make_dimnames(dim(x))
          grp <- as.factor(sample(rep_len(1:sqrt(nrow(x)), nrow(x))))
          levels(grp) <- letters[sample(1:nlevels(grp))]
          
          maxfreq <- max(tabulate(unclass(grp)))
          out.dim <- c(dim(x), length(unique(grp)))
          out.dim[margin] <- maxfreq
          fillvalue <- datagens[[iType]]()[1L]
          
          out.dimnames <- NULL
          if(iNamed) {
            out.dimnames <- rep(NULL, length(out.dim))
            out.dimnames[1:ndim(x)] <- dimnames(x)
            out.dimnames[margin] <- list(NULL)
            out.dimnames[[ndim(x) + 1]] <- levels(grp)
          }
          
          out <- array(fillvalue, out.dim, out.dimnames)
          
          for(k in 1:nlevels(grp)) {
            extract <- x[grp == levels(grp)[k], drop = FALSE]
            out[, , k] <- extract
          }
          
          if(iNamed) {
            expect_equal(
              acast(x, margin, grp),
              out
            ) |> errorfun()
          }
          else {
            expect_equivalent(
              acast(x, margin, grp),
              out
            ) |> errorfun()
          }
          
        }
        enumerate <- enumerate + 1L
        
    }
    
      
    }
  }
 
}


# 1d ====

for(iType in seq_along(datagens)) {
  for(iFill in c(TRUE, FALSE)) {
    for(iNamed in c(TRUE, FALSE)) {
      for(iSample in 1:5) {
        
        if(iFill) {
          x <- array(datagens[[iType]](), test_make_dims(1L))
          if(iNamed) names(x) <- sample(letters, length(x), TRUE)
          grp <- as.factor(sample(1:(nrow(x)-1L), nrow(x), TRUE))
          if(nlevels(grp) >= 2L) {
            levels(grp) <- letters[sample(1:nlevels(grp))]
            
            maxfreq <- max(tabulate(unclass(grp)))
            out.dim <- c(dim(x), length(unique(grp)))
            out.dim[margin] <- maxfreq
            
            out.dimnames <- NULL
            if(iNamed) out.dimnames <- list(NULL, levels(grp))
            
            fillvalue <- test_make_fillval(x)
            
            out <- array(fillvalue, out.dim, out.dimnames)
            
            for(k in 1:nlevels(grp)) {
              extract <- x[grp == levels(grp)[k], drop=FALSE]
              out[1:nrow(extract), k] <- extract
            }
            
            if(iNamed) {
              expect_equal(
                acast(x, margin, grp, TRUE, fillvalue),
                out
              ) |> errorfun()
            }
            else {
              expect_equivalent(
                acast(x, margin, grp, TRUE, fillvalue),
                out
              ) |> errorfun()
            }
          }
          
        }
        
        else if(!iFill) {
          if(iNamed) x <- array(datagens[[iType]](), test_make_dims(1L))
          names(x) <- sample(letters, length(x), TRUE)
          grp <- as.factor(sample(rep_len(1:sqrt(nrow(x)), nrow(x))))
          levels(grp) <- letters[sample(1:nlevels(grp))]
          
          maxfreq <- max(tabulate(unclass(grp)))
          out.dim <- c(dim(x), length(unique(grp)))
          out.dim[margin] <- maxfreq
          
          out.dimnames <- NULL
          if(iNamed) out.dimnames <- list(NULL, levels(grp))
          
          fillvalue <- test_make_fillval(x)
          out <- array(fillvalue, out.dim, out.dimnames)
          
          for(k in 1:nlevels(grp)) {
            extract <- x[grp == levels(grp)[k], drop = FALSE]
            out[, k] <- extract
          }
          
          if(iNamed) {
            expect_equal(
              acast(x, margin, grp),
              out
            ) |> errorfun()
          }
          else {
            expect_equivalent(
              acast(x, margin, grp),
              out
            ) |> errorfun()
          }
          
        }
        enumerate <- enumerate + 1L
        
        
      }
    }
    
  }
  
}

