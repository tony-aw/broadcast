
# recursive vector ====
x <- list(
  setNames(1:11, letters[1:11]), 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
)
names(x) <- month.abb
print(x)

cast_shallow2atomic(x, 0L)
cast_shallow2atomic(x, 1L, comnames_from = 1L)
cast_shallow2atomic(x, -1L, comnames_from = 1L)


# recursive matrix ====
x <- list(
  setNames(1:11, letters[1:11]), 1:10, 1:9, 1:8, 1:7, 1:6, 1:5, 1:4, 1:3, 1:2, 1L, integer(0L)
) |> rev()
dim(x) <- c(3, 4)
dimnames(x) <- list(month.abb[1:3], month.name[1:4])
print(x)

cast_shallow2atomic(x, 0L)
cast_shallow2atomic(x, 1L, comnames_from = length(x))
cast_shallow2atomic(x, -1L, comnames_from = length(x))

