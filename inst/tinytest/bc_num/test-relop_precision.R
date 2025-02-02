
# set-up ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

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

expect_equal(bc.d(x, y, "d==") |> as.vector(), equal)
expect_equal(bc.d(x, y, "d<=") |> as.vector(), equal | smaller)
expect_equal(bc.d(x, y, "d>=") |> as.vector(), equal | bigger)
expect_equal(bc.d(x, y, "d!=") |> as.vector(), !equal)
expect_equal(bc.d(x, y, "d<") |> as.vector(), !equal & smaller)
expect_equal(bc.d(x, y, "d>") |> as.vector(), !equal & bigger)

enumerate <- enumerate + 6L


# relational checks ====
expect_equal(bc.d(x, y, "d!="), !(bc.d(x, y, "d==")))
expect_equal(bc.d(x, y, "d<="), !(bc.d(x, y, "d>")))
expect_equal(bc.d(x, y, "d>="), !(bc.d(x, y, "d<")))

enumerate <- enumerate + 3L


# Regular Integer checks ====
d <- sample(1:10)
d <- expand.grid(d, d)
d1 <- d[, 1] |> as.double() |> as.array()
d2 <- d[, 2] |> as.double() |> as.array()
expect_equal(
  as.vector(d1 == d2),
  bc.d(d1, d2, "d==") |> as.vector()
)
expect_equal(
  as.vector(d1 != d2),
  bc.d(d1, d2, "d!=") |> as.vector()
)
expect_equal(
  as.vector(d1 <= d2),
  bc.d(d1, d2, "d<=") |> as.vector()
)
expect_equal(
  as.vector(d1 >= d2),
  bc.d(d1, d2, "d>=") |> as.vector()
)
expect_equal(
  as.vector(d1 < d2),
  bc.d(d1, d2, "d<") |> as.vector()
)
expect_equal(
  as.vector(d1 > d2),
  bc.d(d1, d2, "d>") |> as.vector()
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
out <- bc.d(d1, d2, "d==") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 != d2)
expect[all_inf] <- NA
out <- bc.d(d1, d2, "d!=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 <= d2)
expect[all_inf] <- NA
out <- bc.d(d1, d2, "d<=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 >= d2)
expect[all_inf] <- NA
out <- bc.d(d1, d2, "d>=") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 < d2)
expect[all_inf] <- NA
out <- bc.d(d1, d2, "d<") |> as.vector()
expect_equal(
  expect, out
)

expect <- as.vector(d1 > d2)
expect[all_inf] <- NA
out <- bc.d(d1, d2, "d>") |> as.vector()
expect_equal(
  expect, out
)

enumerate <- enumerate + 6L
