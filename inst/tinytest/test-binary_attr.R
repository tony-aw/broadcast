# test binary attributes
enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

random_attr <- list(
  c(TRUE, FALSE, NA),
  1:10,
  rnorm(10),
  rnorm(10) + rnorm(10) * -1i,
  month.name,
  as.raw(1:10)
)
random_attr <- c(random_attr, list(random_attr))
n.attr <- length(random_attr)


for(iSample in 1:5) {
  for(iLenX in c(1, 10)) {
    for(iLenY in c(1, 10)) {
      for(iDimX in c(TRUE, FALSE)) {
        for(iDimY in c(TRUE, FALSE)) {
          for(iAttrX in 0:2) {
            for(iAttrY in 0:2) {
              x <- rnorm(iLenX)
              y <- rnorm(iLenY)
              
              if(length(x) > 1 && iDimX) {
                x <- rep_len(x, 20)
                dim(x) <- c(10, 2)
              }
              if(length(y) > 1 && iDimY) {
                y <- rep_len(y, 20)
                dim(y) <- c(10, 2)
              }
              if(iAttrX) {
                ind <- sample(1:n.attr, 1L)
                attr(x, "test") <- random_attr[[ind]]
                if(iAttrX > 1) {
                  ind <- sample(1:n.attr, 1L)
                  attr(x, "test2") <- random_attr[[ind]]
                }
              }
              if(iAttrY) {
                ind <- sample(1:n.attr, 1L)
                attr(y, "test") <- random_attr[[ind]]
                if(iAttrY > 1) {
                  ind <- sample(1:n.attr, 1L)
                  attr(y, "test2") <- random_attr[[ind]]
                }
              }
              class(x) <- "mutatomic"
              class(y) <- "mutatomic"
              
              expect_equal(
                x + y,
                bc(~ x + y)
              ) |> errorfun()
              
              enumerate <- enumerate + 1L
              
            }
          }
        }
      }
    }
  }
}


