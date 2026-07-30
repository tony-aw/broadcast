
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

library(squarebrackets)

x <- array(1:10, c(1, 10))
y <- array(1:10, c(1, 10))

for(iMutX in c(TRUE, FALSE)) {
  for(iMutY in c(TRUE, FALSE)) {
    for(iBcX in c(TRUE, FALSE)) {
      for(iBcY in c(TRUE, FALSE)) {
        for(iCastX in c(TRUE, FALSE)) {
          for(iCastY in c(TRUE, FALSE)) {
            x2 <- x
            y2 <- y
            
            if(iMutX) {
              x2 <- as.mutatomic(x2)
            }
            if(iMutY) {
              y2 <- as.mutatomic(y2)
            }
            
            
            if(iCastX) x2 <- as_dbl(x2)
            if(iCastY) y2 <- as_dbl(y2)
            
            broadcaster(x2) <- iBcX
            broadcaster(y2) <- iBcY
            
            if(iMutX || iMutY) {
              expect_true(
                is.mutatomic(x2 + y2)
              ) |> errorfun()
              expect_true(
                is.mutatomic(bc_strrep(as_chr(x2), as_int(y2)))
              ) |> errorfun()
              cond <- as.mutatomic(x2 | y2)
              expect_true(
                is.mutatomic(bc_ifelse(cond, x2, y2))
              ) |> errorfun()
              
              # expect_true(
              #   is.mutatomic(bind_array(list(x2, y2), 1L))
              # ) |> errorfun()
              enumerate <- enumerate + 2L
              
            }
            if(iBcX || iBcY) {
              expect_true(
                broadcaster(x2 + y2)
              ) |> errorfun()
              expect_true(
                broadcaster(x2 == y2)
              ) |> errorfun()
              expect_true(
                broadcaster(bind_array(list(x2, y2), 1L))
              ) |> errorfun()
              enumerate <- enumerate + 3L
            }
            
          }
        }
        
      }
    }
  }
}

