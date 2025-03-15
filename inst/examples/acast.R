
x <- cbind(id = c(rep(1:3, each = 2), 1), grp = c(rep(1:2, 3), 2), val = rnorm(7))
grp <- as.factor(x[, 2])
levels(grp) <- c("a", "b")
margin <- 1L
out <- acast(x, margin, grp, TRUE)