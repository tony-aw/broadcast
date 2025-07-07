

x <- array(c(as.list(1:34), as.list(letters)), 5:3)
dimnames(x) <- list(
  letters[1:5],
  LETTERS[1:4],
  month.abb[1:3]
)
print(x)


# cast `x` from in to out, and distribute names:
x2 <- cast_dim2hier(x, distr.names = TRUE)
head(x2, n = 2)

# cast `x` from out to in, and distribute names:
x2 <- cast_dim2hier(x, in2out = FALSE, distr.names = TRUE)
head(x2, n = 2)
