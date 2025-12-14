


# balanced acasting ====
x <- cbind(id = rep(1:3, each = 2), grp = rep(1:2, 3), val = rnorm(6))
print(x)

grp <- as.factor(x[, 2])
levels(grp) <- c("a", "b")
margin <- 1L

acast(x, margin, grp)



# unbalanced acasting ====
x <- cbind(id = c(rep(1:3, each = 2), 1), grp = c(rep(1:2, 3), 2), val = rnorm(7))
print(x)

grp <- as.factor(x[, 2])
levels(grp) <- c("a", "b")
margin <- 1L

acast(x, margin, grp, fill = TRUE)



# unbalanced acasting with raw array ====
x <- cbind(id = c(rep(1:3, each = 2), 1), grp = c(rep(1:2, 3), 2), val = sample(1:7))
x <- as_raw(x)
print(x)

grp <- x[, 2] |> as.integer() |> as.factor()
levels(grp) <- c("a", "b")
margin <- 1L

(fill_val <- as.raw(255))
acast(x, margin, grp, fill = TRUE, fill_val = fill_val)

