
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

library(squarebrackets)

x <- 1:10
y <- array(1:10, c(1, 10))

for(iMutX in c(TRUE, FALSE)) {
  for(iMutY in c(TRUE, FALSE)) {
    for(iBcX in c(TRUE, FALSE)) {
      for(iBcY in c(TRUE, FALSE)) {
        x2 <- x
        y2 <- y
        
        if(iMutX) {
          x2 <- as.mutatomic(x2)
        }
        if(iMutY) {
          y2 <- as.mutatomic(y2)
        }
        
        broadcaster(x2) <- iBcX
        broadcaster(y2) <- iBcY
        
        if(is.mutatomic(x2) || is.mutatomic(y2)) {
          expect_true(
            is.mutatomic(x2 + y2)
          ) |> errorfun()
          enumerate <- enumerate + 1L
        }
        if(broadcaster(x2) || broadcaster(y2)) {
          expect_true(
            broadcaster(x2 + y2)
          ) |> errorfun()
          enumerate <- enumerate + 1L
        }
        
        
        
      }
    }
  }
}

