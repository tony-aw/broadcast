

# check for each element in one recursive array if values are present in another:
mylist <- list(
  as.raw(0:255),
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  0:255,
  rnorm(10),
  rnorm(10) + rnorm(10) * -1i,
  sample(month.abb)
)
mylist <- c(mylist, list(mylist))
x <- array(sample(mylist, 50, TRUE), c(5, 5, 2))
y <- array(sample(mylist, 50, TRUE), c(5, 5, 2))

bcapply(x, y, `%in%`) # returns a dimensional list / recursive array

bcapply(x, y, \(x, y) any(x %in% y), v = "logical") # returns logical array

bcapply(x, y, \(x, y) all(x %in% y), v = "logical") # returns logical array



# calculate quartiles for each list item, and return numeric array of quartiles:
x <- list(
  a = 1:10,
  beta = exp(-3:3),
  logic = c(TRUE,FALSE,FALSE,TRUE)
)
print(x)
quantiles <- array(c(1:3/4), c(1, 3))
colnames(quantiles) <- paste0("q = ", quantiles)
print(quantiles)

out <- bcapply(x, quantiles, \(x, y) quantile(x, probs = y), v = "double")
print(out)
typeof(out)
