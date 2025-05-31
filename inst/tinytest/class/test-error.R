
enumerate <- 0 # to count number of tests


x <- factor(letters)
expect_error(
  broadcaster(x) <- TRUE,
  "cannot make this object broadcaster"
)
x <- data.frame(letters, 1:26)
expect_error(
  broadcaster(x) <- TRUE,
  "cannot make this object broadcaster"
)

enumerate <- enumerate + 2L

