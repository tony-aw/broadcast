
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
tol <- sqrt(.Machine$double.eps)
eps <- tol * 10 # large eps relative to tolerance, because Ubuntu's precision is bad

testfun <- function(x, y, op, tol = sqrt(.Machine$double.eps)) {
  out <- bc.d(x, y, op, tol = tol)
  out[is.na(out)] <- NA # because Ubuntu doesn't handle NaN and NA properly
  return(out)
}

# basic checks ====

x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps,
  c(0.3, 0.6, 0.7) - eps,
  NA, NaN, NA,
  c(0.3, 0.6, 0.7)
) |> as.array()
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps,
  c(0.1*3, 0.1*6, 0.1*7),
  NA, NaN, NA
) |> as.array()

equal <- c(rep(TRUE, 3), rep(FALSE, 6), rep(NA, 6))
smaller <- c(rep(FALSE, 6), rep(TRUE, 3), rep(NA, 6))
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3), rep(NA, 6))

out <- testfun(x, y, "d==", tol = tol)
print(out)
print(equal)
expect_equal(out |> as.vector(), equal)
expect_equal(testfun(x, y, "d<=") |> as.vector(), equal | smaller)
expect_equal(testfun(x, y, "d>=") |> as.vector(), equal | bigger)
expect_equal(testfun(x, y, "d!=") |> as.vector(), !equal)
expect_equal(testfun(x, y, "d<") |> as.vector(), !equal & smaller)
expect_equal(testfun(x, y, "d>") |> as.vector(), !equal & bigger)

enumerate <- enumerate + 6L


# relational checks ====
expect_equal(testfun(x, y, "d!="), !(testfun(x, y, "d==")))
expect_equal(testfun(x, y, "d<="), !(testfun(x, y, "d>")))
expect_equal(testfun(x, y, "d>="), !(testfun(x, y, "d<")))

enumerate <- enumerate + 3L


# Regular Integer checks ====
d <- sample(1:10)
d <- expand.grid(d, d)
d1 <- d[, 1] |> as.double() |> as.array()
d2 <- d[, 2] |> as.double() |> as.array()
expect_equal(
  as.vector(d1 == d2),
  testfun(d1, d2, "d==") |> as.vector()
)
expect_equal(
  as.vector(d1 != d2),
  testfun(d1, d2, "d!=") |> as.vector()
)
expect_equal(
  as.vector(d1 <= d2),
  testfun(d1, d2, "d<=") |> as.vector()
)
expect_equal(
  as.vector(d1 >= d2),
  testfun(d1, d2, "d>=") |> as.vector()
)
expect_equal(
  as.vector(d1 < d2),
  testfun(d1, d2, "d<") |> as.vector()
)
expect_equal(
  as.vector(d1 > d2),
  testfun(d1, d2, "d>") |> as.vector()
)

enumerate <- enumerate + 6L


# NA/NaN/Inf/-Inf/pi checks ====

d <- c(NA, NaN, Inf, -Inf, pi, 1:10)
d <- expand.grid(d, d)
d1 <- d[,1] |> as.double() |> as.array()
d2 <- d[,2] |> as.double() |> as.array()
all_inf <- which(is.infinite(d1) & is.infinite(d2) & sign(d1) == sign(d2))

expect <- as.vector(d1 == d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d==") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 != d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d!=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 <= d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d<=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 >= d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d>=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 < d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d<") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 > d2)
expect[all_inf] <- NA
out <- testfun(d1, d2, "d>") |> as.vector()
expect_equal(
  expect, out
)

enumerate <- enumerate + 6L



# varying tolerance checks ====
b <- sample(1:6)
p <- sample(-5:-10)
tol <- b*10^p

for(i in tol) {
  eps <- i * 10
  
  x <- c(
    c(0.3, 0.6, 0.7),
    c(0.3, 0.6, 0.7) + eps,
    c(0.3, 0.6, 0.7) - eps,
    NA, NaN, NA,
    c(0.3, 0.6, 0.7)
  ) |> as.array()
  y <- c(
    c(0.1*3, 0.1*6, 0.1*7),
    c(0.1*3, 0.1*6, 0.1*7) - eps,
    c(0.1*3, 0.1*6, 0.1*7) + eps,
    c(0.1*3, 0.1*6, 0.1*7),
    NA, NaN, NA
  ) |> as.array()
  
  equal <- c(rep(TRUE, 3), rep(FALSE, 6), rep(NA, 6))
  smaller <- c(rep(FALSE, 6), rep(TRUE, 3), rep(NA, 6))
  bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3), rep(NA, 6))
  
  out <- testfun(x, y, "d==", tol = i)
  print(out)
  print(equal)
  expect_equal(out |> as.vector(), equal) |> errorfun()
  expect_equal(testfun(x, y, "d<=", tol = i) |> as.vector(), equal | smaller) |> errorfun()
  expect_equal(testfun(x, y, "d>=", tol = i) |> as.vector(), equal | bigger) |> errorfun()
  expect_equal(testfun(x, y, "d!=", tol = i) |> as.vector(), !equal) |> errorfun()
  expect_equal(testfun(x, y, "d<", tol = i) |> as.vector(), !equal & smaller) |> errorfun()
  expect_equal(testfun(x, y, "d>", tol = i) |> as.vector(), !equal & bigger) |> errorfun()
  
  enumerate <- enumerate + 6L
  
}



# error checks ====
x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps,
  c(0.3, 0.6, 0.7) - eps,
  NA, NaN, NA,
  c(0.3, 0.6, 0.7)
) |> as.array()
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps,
  c(0.1*3, 0.1*6, 0.1*7),
  NA, NaN, NA
) |> as.array()

expect_error(
  bc.d(x, y, "==", tol = 1),
  pattern = "`tol` must be >= 0 and <= 0.1",
  fixed = TRUE
)

expect_error(
  bc.d(x, y, "==", tol = rep(1e-5, 10)),
  pattern = "`tol` must be a single decimal number",
  fixed = TRUE
)

expect_error(
  bc.d(x, y, "==", tol = "0.01"),
  pattern = "`tol` must be a single decimal number",
  fixed = TRUE
)
enumerate <- enumerate + 3L

