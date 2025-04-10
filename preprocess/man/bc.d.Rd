% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bc_d.R
\name{bc.d}
\alias{bc.d}
\alias{bc.num}
\title{Broadcasted Decimal Numeric Operations}
\usage{
bc.d(x, y, op, tol = sqrt(.Machine$double.eps))

bc.num(x, y, op, tol = sqrt(.Machine$double.eps))
}
\arguments{
\item{x, y}{conformable logical or numeric arrays.}

\item{op}{a single string, giving the operator. \cr
Supported arithmetic operators: +, -, *, /, ^, pmin, pmax. \cr
Supported relational operators: ==, !=, <, >, <=, >=, d==, d!=, d<, d>, d<=, d>=. \cr}

\item{tol}{a single number between 0 and 0.1, giving the machine tolerance to use. \cr
Only relevant for the following operators: \cr
d==, d!=, d<, d>, d<=, d>= \cr
See the
\%d==\%, \%d!=\%, \%d<\%, \%d>\%, \%d<=\%, \%d>=\% operators
from the 'tinycodet' package for details. \cr}
}
\value{
For arithmetic operators: \cr
A numeric array as a result of the broadcasted decimal arithmetic operation. \cr
\cr
For relational operators: \cr
A logical array as a result of the broadcasted decimal relational comparison. \cr
\cr
}
\description{
The \code{bc.d()} function
performs broadcasted decimal numeric operations on 2 numeric or logical arrays. \cr
\code{bc.num()} is an alias for \code{bc.d()}. \cr \cr
}
\examples{
x.dim <- c(4:2)
x.len <- prod(x.dim)
x.data <- sample(c(NA, 1.1:1000.1), x.len, TRUE)
x <- array(x.data, x.dim)
y <- array(1:50, c(4,1,1))

bc.d(x, y, "+")
bc.d(x, y, "-")
bc.d(x, y, "*")
bc.d(x, y, "/")
bc.d(x, y, "^")

bc.d(x, y, "==")
bc.d(x, y, "!=")
bc.d(x, y, "<")
bc.d(x, y, ">")
bc.d(x, y, "<=")
bc.d(x, y, ">=")
}
