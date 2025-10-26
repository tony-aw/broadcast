
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# no dimnames ====
x <- rep(list(1:11), 10)
dim(x) <- c(5, 2)

expected <- do.call(cbind, x)
dim(expected) <- c(11, 5, 2)
expect_equal(
  cast_shallow2atomic(x, 1),
  expected
)
expected <- do.call(rbind, x)
dim(expected) <- c(5, 2, 11)
expect_equal(
  cast_shallow2atomic(x, -1),
  expected
)
enumerate <- enumerate + 2L


# comnames_from with equal lengths ====
x.orig <- list(
  setNames(1:11, letters[1:11]),
  1:11,
  setNames(1:11, month.abb[1:11]),
  1:11,
  setNames(1:11, LETTERS[1:11]),
  1:11
)
dim(x.orig) <- c(3,2)

for(i in 1:length(x.orig)) {
  for(j in c(TRUE, FALSE)) {
    for(k in c(TRUE, FALSE)) {
      x <- x.orig
      
      if(j || k) {
        dimnames(x) <- list(month.name[1:3], LETTERS[1:2])
        dimnames(x)[!k] <- list(NULL)
        dimnames(x)[!j] <- list(NULL)
      }
      nms <- names(x[[i]])
      
      expected <- cbind(1:11, 1:11, 1:11, 1:11, 1:11, 1:11)
      dim(expected) <- c(11, dim(x))
      if(!is.null(nms) || !is.null(dimnames(x))) {
        x.dimnames <- dimnames(x)
        if(is.null(x.dimnames)) x.dimnames <- rep(list(NULL), ndim(x))
        dimnames(expected) <- c(list(nms), x.dimnames)
      }
      expect_equal(
        cast_shallow2atomic(x, 1, comnames_from = i),
        expected
      ) |> errorfun()
      
      expected <- rbind(1:11, 1:11, 1:11, 1:11, 1:11, 1:11)
      dim(expected) <- c(dim(x), 11)
      if(!is.null(nms) || !is.null(dimnames(x))) {
        x.dimnames <- dimnames(x)
        if(is.null(x.dimnames)) x.dimnames <- rep(list(NULL), ndim(x))
        dimnames(expected) <- c(x.dimnames, list(nms))
      }
      expect_equal(
        cast_shallow2atomic(x, -1, comnames_from = i),
        expected
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
    }
    
    
  }
  
}



# comnames_from with unequal lengths ====
x.orig <- list(
  setNames(1:11, letters[1:11]),
  1:10,
  setNames(1:9, month.abb[1:9]),
  1:8,
  setNames(1:7, LETTERS[1:7]),
  1:6
)
dim(x.orig) <- c(3,2)

for(i in 1:length(x.orig)) {
  for(j in c(TRUE, FALSE)) {
    for(k in c(TRUE, FALSE)) {
      
      # set-up:
      x <- x.orig
      if(j || k) {
        dimnames(x) <- list(month.name[1:3], LETTERS[1:2])
        dimnames(x)[!k] <- list(NULL)
        dimnames(x)[!j] <- list(NULL)
      }
      nms <- NULL
      if(i == 1L) {
        nms <- names(x[[i]])
      }
      
      # test arrangement = 1L:
      expected <- lapply(11:6, \(i)1:i)
      expected <- lapply(expected, \(x) {
        length(x) <- 11
        x
      })
      expected <- do.call(cbind, expected)
      dim(expected) <- c(11, dim(x))
      if(!is.null(nms) || !is.null(dimnames(x))) {
        x.dimnames <- dimnames(x)
        if(is.null(x.dimnames)) x.dimnames <- rep(list(NULL), ndim(x))
        dimnames(expected) <- c(list(nms), x.dimnames)
      }
      expect_equal(
        cast_shallow2atomic(x, 1, comnames_from = i),
        expected
      ) |> errorfun()
      
      
      # test arrangement = -1L:
      expected <- lapply(11:6, \(i)1:i)
      expected <- lapply(expected, \(x) {
        length(x) <- 11
        x
      })
      expected <- do.call(rbind, expected)
      dim(expected) <- c(dim(x), 11)
      if(!is.null(nms) || !is.null(dimnames(x))) {
        x.dimnames <- dimnames(x)
        if(is.null(x.dimnames)) x.dimnames <- rep(list(NULL), ndim(x))
        dimnames(expected) <- c(x.dimnames, list(nms))
      }
      expect_equal(
        cast_shallow2atomic(x, -1, comnames_from = i),
        expected
      ) |> errorfun()
      
      
      
      # enumerate:
      enumerate <- enumerate + 2L
      
    }
  }
  
}
