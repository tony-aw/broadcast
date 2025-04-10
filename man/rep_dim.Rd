% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rep_dim.R
\name{rep_dim}
\alias{rep_dim}
\title{Replicate Array Dimensions}
\usage{
rep_dim(x, tdim)
}
\arguments{
\item{x}{an atomic or recursive array or matrix.}

\item{tdim}{an integer vector, giving the target dimension to reach.}
}
\value{
Returns the replicated array.
}
\description{
The \code{rep_dim()} function
replicates array dimensions until the specified dimension sizes are reached,
and returns the array. \cr
\cr
The various broadcasting functions recycle array dimensions virtually,
meaning little to no additional memory is needed. \cr
The \code{rep_dim()} function,
however,
physically replicates the dimensions of an array
(and thus actually occupies additional memory space). \cr
\cr
}
\examples{

x <- matrix(1:9, 3,3)
colnames(x) <- LETTERS[1:3]
rownames(x) <- letters[1:3]
names(x) <- month.abb[1:9]
print(x)

rep_dim(x, c(3,3,2)) # replicate to larger size
}
