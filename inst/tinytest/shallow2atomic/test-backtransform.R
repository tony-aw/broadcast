
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
x <- lapply(1:10, \(i)sample(1:10))
out <- cast_shallow2atomic(x, 1)
out <- asplit(out, seq(2, ndim(out)), drop = TRUE) |> undim()
expect_equivalent(x, out)

x <- lapply(1:10, \(i)sample(1:10))
out <- cast_shallow2atomic(x, -1)
out <- asplit(out, seq(1, ndim(out) - 1), drop = TRUE) |> undim()
expect_equivalent(x, out)

enumerate <- enumerate + 2L


# with names ====
x.orig <- list(
  setNames(1:11, letters[1:11]),
  1:11,
  setNames(1:11, month.abb[1:11]),
  1:11,
  setNames(1:11, LETTERS[1:11]),
  1:11
)
dim(x.orig) <- c(3,2)


for(i in 1:3) {
  for(j in c(TRUE, FALSE)) {
    for(k in c(TRUE, FALSE)) {
      x <- x.orig
      
      if(j || k) {
        dimnames(x) <- list(month.name[1:3], LETTERS[1:2])
        dimnames(x)[!k] <- list(NULL)
        dimnames(x)[!j] <- list(NULL)
      }
    
      out <- cast_shallow2atomic(x, 1, comnames_from = i)
      out <- asplit(out, seq(2, ndim(out)))
      out <- lapply(out, undim)
      expect_equivalent(x, out) |> errorfun()
      
      
      out <- cast_shallow2atomic(x, -1, comnames_from = i)
      out <- asplit(out, seq(1, ndim(out) - 1))
      out <- lapply(out, undim)
      expect_equivalent(x, out) |> errorfun()
      
      
      enumerate <- enumerate + 2L
    }
  }
  
}

