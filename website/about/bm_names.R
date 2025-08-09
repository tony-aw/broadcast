
# set-up ====
library(broadcast)


# arrays ====
n <- 400
x <- x2 <- array(rnorm(100), c(n, 1, n))
y <- y2 <- array(rnorm(100), c(1, n, 1))
dimnames(x2) <- lapply(dim(x2), \(m) sample(letters, m, TRUE))
dimnames(y2) <- lapply(dim(y2), \(m) sample(letters, m, TRUE))
bm_names_arrays <- bench::mark(
  unnamed = bc.d(x, y, "+"),
  named = bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(bm_names_arrays)
plot(bm_names_arrays)

n <- 10
x <- x2 <- array(rnorm(100), c(n, 1, n))
y <- y2 <- array(rnorm(100), c(1, n, 1))
dimnames(x2) <- lapply(dim(x2), \(m) sample(letters, m, TRUE))
dimnames(y2) <- lapply(dim(y2), \(m) sample(letters, m, TRUE))
overhead_names_arrays <- bench::mark(
  unnamed = for(i in 1:100) bc.d(x, y, "+"),
  named = for(i in 1:100) bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(overhead_names_arrays)
plot(overhead_names_arrays)


# array and vector ====
n <- 400
x <- x2 <- array(rnorm(100), c(n, 1, n))
y <- y2 <- rnorm(n)
dimnames(x2) <- lapply(dim(x2), \(m) sample(letters, m, TRUE))
names(y2) <- sample(letters, n, TRUE)
bm_names_av <- bench::mark(
  unnamed = bc.d(x, y, "+"),
  named = bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(bm_names_av)
plot(bm_names_av)

n <- 10
x <- x2 <- array(rnorm(100), c(n, 1, n))
y <- y2 <- rnorm(n)
dimnames(x2) <- lapply(dim(x2), \(m) sample(letters, m, TRUE))
names(y2) <- sample(letters, n, TRUE)
overhead_names_av <- bench::mark(
  unnamed = for(i in 1:100) bc.d(x, y, "+"),
  named = for(i in 1:100) bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(overhead_names_av)
plot(overhead_names_av)


# 1d ====
n <- 1e7
x <- x2 <- array(rnorm(n))
y <- y2 <- array(rnorm(n))
names(x2) <- sample(letters, n, TRUE)
names(y2) <- sample(letters, n, TRUE)
bm_names_1d <- bench::mark(
  unnamed = bc.d(x, y, "+"),
  named = bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(bm_names_1d)
plot(bm_names_1d)

n <- 10
x <- x2 <- array(rnorm(n))
y <- y2 <- array(rnorm(n))
names(x2) <- sample(letters, n, TRUE)
names(y2) <- sample(letters, n, TRUE)
overhead_names_1d <- bench::mark(
  unnamed = for(i in 1:100) bc.d(x, y, "+"),
  named = for(i in 1:100) bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(overhead_names_1d)
plot(overhead_names_1d)


# vectors ====
n <- 1e7
x <- x2 <- rnorm(n)
y <- y2 <- rnorm(n)
names(x2) <- sample(letters, n, TRUE)
names(y2) <- sample(letters, n, TRUE)
bm_names_vectors <- bench::mark(
  unnamed = bc.d(x, y, "+"),
  named = bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(bm_names_vectors)
plot(bm_names_vectors)


load("overhead_names_vectors_old.RData")
overhead_names_vectors_old <- overhead_names_vectors
n <- 10
x <- x2 <- rnorm(n)
y <- y2 <- rnorm(n)
names(x2) <- sample(letters, n, TRUE)
names(y2) <- sample(letters, n, TRUE)
overhead_names_vectors <- bench::mark(
  unnamed = for(i in 1:100) bc.d(x, y, "+"),
  named = for(i in 1:100) bc.d(x2, y2, "+"),
  min_iterations = 100,
  check = FALSE
)
summary(overhead_names_vectors)
summary(overhead_names_vectors_old)
plot(overhead_names_vectors)
save(overhead_names_vectors, file = "overhead_names_vectors.RData")


