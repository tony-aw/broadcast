
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- cbind(id = c(rep(1:3, each = 2), 1), grp = c(rep(1:2, 3), 2), val = rnorm(7))
broadcaster(x) <- TRUE

grp <- as.factor(x[, 2])
levels(grp) <- c("a", "b")
margin <- 1L

a <- rbind(x[grp == "a", ], NA)
b <- x[grp == "b", ]
expected <- bind_array(
  list(a = a, b = b), along = 3L
)

out <- acast(x, margin, grp, fill = TRUE)

expect_equal(
  out,
  expected
)

expect_true(
  broadcaster(out)
)

enumerate <- enumerate + 2L
