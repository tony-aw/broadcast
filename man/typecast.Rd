% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/typecast.R
\name{typecast}
\alias{typecast}
\alias{as_bool}
\alias{as_int}
\alias{as_dbl}
\alias{as_num}
\alias{as_chr}
\alias{as_str}
\alias{as_cplx}
\alias{as_raw}
\alias{as_list}
\title{Atomic and List Type Casting With Names and Dimensions Preserved}
\usage{
as_bool(x, ...)

as_int(x, ...)

as_dbl(x, ...)

as_num(x, ...)

as_chr(x, ...)

as_str(x, ...)

as_cplx(x, ...)

as_raw(x, ...)

as_list(x, ...)
}
\arguments{
\item{x}{an R object.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The converted object. \cr \cr
}
\description{
Type casting usually strips away attributes of objects. \cr
The functions provided here preserve dimensions, dimnames, and names,
which may be more convenient for arrays and array-like objects. \cr
\cr
The functions are as follows: \cr
\itemize{
\item \code{as_bool()}: converts object to atomic type \code{logical} (\code{TRUE, FALSE, NA}).
\item \code{as_int()}: converts object to atomic type \code{integer}.
\item \code{as_dbl()}: converts object to atomic type \code{double} (AKA numeric).
\item \code{as_cplx()}: converts object to atomic type \code{complex}.
\item \code{as_chr()}: converts object to atomic type \code{character}.
\item \code{as_raw()}: converts object to atomic type \code{raw}.
\item \code{as_list()}: converts object to recursive type \code{list}. \cr
}

\code{as_num()} is an alias for \code{as_dbl()}. \cr
\code{as_str()} is an alias for \code{as_chr()}. \cr
\cr
See also \link[base]{typeof}. \cr \cr
}
\examples{

# matrix example ====
x <- matrix(sample(-1:28), ncol = 5)
colnames(x) <- month.name[1:5]
rownames(x) <- month.abb[1:6]
names(x) <- c(letters[1:20], LETTERS[1:10])
print(x)

as_bool(x)
as_int(x)
as_dbl(x)
as_chr(x)
as_cplx(x)
as_raw(x)


################################################################################

# factor example ====
x <- factor(month.abb, levels = month.abb)
names(x) <- month.name
print(x)

as_bool(as_int(x) > 6)
as_int(x)
as_dbl(x)
as_chr(x)
as_cplx(x)
as_raw(x)
}
