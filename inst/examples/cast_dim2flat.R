
x <- array(
  sample(list(letters, month.name, 1:10 ~ "foo"), prod(4:2), TRUE),
  dim = 4:2,
  dimnames = list(NULL, LETTERS[1:3], c("x", "y"))
)


# summarizing ====
summary(x) # dimensional information is lost

# In the following instances, dimensional position info is retained:
cast_dim2flat(x) |> summary()

cast_dim2flat(x[1:3, 1:2, 2, drop = FALSE]) |> summary()

cast_dim2flat(x[1:3, 1:2, 2, drop = TRUE]) |> summary()


# printing ====
print(x) # too compact
cast_dim2flat(x) |> print() # much less compact


