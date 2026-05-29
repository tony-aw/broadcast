
# string concatenation:
x <- array(letters, c(10, 2, 1))
y <- array(letters, c(10,1,1))
bc.str(x, y, "+")


# string (in)equality:
bc.str(array(letters), array(letters), "==")
bc.str(array(letters), array(letters), "!=")


# string distances:
x <- setNames(month.name, month.name) |> vector2array(1L)
y <- setNames(month.abb, month.abb) |> vector2array(2L)
bc.str(x, y, "levenshtein") # levenshtein
bc.str(x, y, "lcss") # longest common sub-string
