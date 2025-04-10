% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bc_str.R
\name{bc.str}
\alias{bc.str}
\title{Broadcasted String Operations}
\usage{
bc.str(x, y, op)
}
\arguments{
\item{x, y}{conformable atomic arrays of type \code{character}.}

\item{op}{a single string, giving the operator. \cr
Supported concatenation operators: +. \cr
Supported relational operators: ==, !=. \cr
Supported distance operators: levenshtein. \cr}
}
\value{
For concatenation operation: \cr
A character array as a result of the broadcasted concatenation operation. \cr
\cr
For relational operation: \cr
A logical array as a result of the broadcasted relational comparison. \cr
\cr
For distance operation: \cr
An integer array as a result of the broadcasted distance measurement. \cr
\cr
}
\description{
The \code{bc.str()} function
performs broadcasted string operations on pairs of arrays. \cr \cr
}
\examples{

# string concatenation:
x <- array(letters, c(10, 2, 1))
y <- array(letters, c(10,1,1))
bc.str(x, y, "+")


# string (in)equality:
bc.str(array(letters), array(letters), "==")
bc.str(array(letters), array(letters), "!=")


# string distance (Levenshtein):
x <- array(month.name, c(12, 1))
y <- array(month.abb, c(1, 12))
out <- bc.str(x, y, "levenshtein")
dimnames(out) <- list(month.name, month.abb)
print(out)
}
\references{
The 'C++' code for the Levenshtein edit string distance is based on the code found in
\url{https://rosettacode.org/wiki/Levenshtein_distance#C++}
}
