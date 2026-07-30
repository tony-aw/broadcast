
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# to test:
# - comnames from element 
# - comnames from element with too few elements
# - dimnames from x


# no names ====
x <- rep(list(1:11), 10)
expect_equal(
  cast_shallow2atomic(x, 1),
  simplify2array(x)
)
expect_equal(
  cast_shallow2atomic(x, -1),
  simplify2array(x) |> t()
)
enumerate <- enumerate + 2L


# comnames_from with equal lengths ====
x.orig <- list(
  setNames(1:11, letters[1:11]), 1:11, setNames(1:11, month.abb[1:11])
)

for(i in 1:3) {
  for(j in c(TRUE, FALSE)) {
    x <- x.orig
    if(j) names(x) <- month.name[1:3]
    nms <- names(x[[i]])
    
    expected <- cbind(1:11, 1:11, 1:11)
    if(!is.null(nms) || !is.null(names(x))) {
      dimnames(expected) <- list(nms, names(x))
    }
    expect_equal(
      cast_shallow2atomic(x, 1, comnames_from = i),
      expected
    ) |> errorfun()
    
    expected <- rbind(1:11, 1:11, 1:11)
    if(!is.null(nms) || !is.null(names(x))) {
      dimnames(expected) <- list(names(x), nms)
    }
    expect_equal(
      cast_shallow2atomic(x, -1, comnames_from = i),
      expected
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
    
  }
  
}


# comnames_from with unequal lengths ====
x.orig <- list(
  setNames(1:11, letters[1:11]), setNames(1:10, LETTERS[1:10]), setNames(1:9, month.abb[1:9])
)

for(i in 1:3) {
  for(j in c(TRUE, FALSE)) {
    x <- x.orig
    if(j) names(x) <- month.name[1:3]
    nms <- NULL
    if(i == 1L) {
      nms <- names(x[[i]])
    }
    
    expected <- cbind(1:11, c(1:10, NA), c(1:9, NA, NA))
    if(!is.null(nms) || !is.null(names(x))) {
      dimnames(expected) <- list(nms, names(x))
    }
    expect_equal(
      cast_shallow2atomic(x, 1, comnames_from = i),
      expected
    ) |> errorfun()
    
    expected <- rbind(1:11, c(1:10, NA), c(1:9, NA, NA))
    if(!is.null(nms) || !is.null(names(x))) {
      dimnames(expected) <- list(names(x), nms)
    }
    expect_equal(
      cast_shallow2atomic(x, -1, comnames_from = i),
      expected
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
    
  }
  
}

