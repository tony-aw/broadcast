#' broadcast Package Overview
#' 
#' @description
#' broadcast: \cr
#' Efficient Broadcasted Array Operations Like 'NumPy' \cr \cr
#' 
#' 
#' ```{r echo = FALSE, eval = TRUE, results = 'asis'}
#' 
#' txt <- packageDescription("broadcast", fields = "Description")
#' p <- c("\t", ",\n", ".\n", "\n(", "following.", "    ")
#' rp <- c("", ", ", ".\n\n",  " (", "following:", " ")
#' for(i in 1:length(rp)) {
#'  txt <- gsub(p[i], rp[i], txt, fixed = TRUE)
#' }
#' txt <- paste0(txt, "\\cr \\cr")
#' cat(txt)
#' ```
#' 
#' 
#' @section Links to Get Started:
#'  - The Quick-Start Guide, Vignettes, Benchmarks, and more can be found on the \href{https://tony-aw.github.io/broadcast/}{website}.
#'  - GitHub main page: \url{https://github.com/tony-aw/broadcast}
#'  - Reporting Issues or Giving Suggestions: \url{https://github.com/tony-aw/broadcast/issues} \cr \cr
#' 
#' 
#' @section Methods/Functions:
#' 
#' \bold{Broadcasted Operators} \cr
#' Base 'R' comes with relational (==, !=, etc.),
#' arithmetic (+, -, *, /, etc.), and logical/bit-wise (&, |) operators. \cr
#' 'broadcast' provides 2 ways to use these operators with broadcasting. \cr
#' \cr
#' The first (and simple) way is to use the \link{broadcaster} class,
#' which comes with it's own method dispatch for the above mentioned operators. \cr
#' This method support operator precedence, and for the average 'R' user,
#' this is sufficient. \cr
#' \cr
#' The second way is to use the large set of `bc.`- functions. \cr
#' These offer much greater control and more operators than the previous method,
#' and has less risk of running into conflicting methods. \cr
#' But it does not support operator precedence. \cr
#' \cr
#' More information about both methods can be found here: \cr
#' \link{broadcast_operators}. \cr
#' \cr
#' 
#' 
#' \bold{Binding Arrays} \cr
#' 'broadcast' provides the \link{bind_array} function,
#' to bind arrays along an arbitrary dimension,
#' with support for broadcasting. \cr
#' See \link{bind_array}. \cr\cr
#' 
#'  
#' \bold{Casting Methods} \cr
#' 'broadcast' provides several "casting" methods. \cr
#' These can facility complex forms of broadcasting that would normally not be possible. \cr
#' But these "casting" methods also have their own merit, beside empowering complex broadcasting. \cr
#' \cr
#' More information about the casting methods can be found here: \cr
#' \link{broadcast_casting}. \cr
#' \cr
#' 
#' \bold{General Pairwise Broadcasted Methods} \cr
#' 'broadcast' also comes with a few general pairwise broadcasted methods:
#' 
#'  * \link{bc_ifelse}: Broadcasted version of \link[base]{ifelse}.
#'  * \link{bcapply}: Broadcasted apply-like method.
#'  * \link{bc_strrep}: Broadcasted version of \link[base]{strrep}. \cr \cr
#' 
#' 
#' \bold{Other functions} \cr
#' 'broadcast' provides
#' \link[=as_bool]{type-casting} functions,
#' which preserve names and dimensions - convenient for arrays. \cr
#' \cr
#' 'broadcast' also provides
#' \link[=sd_lc]{simple linear algebra functions for statistics}. \cr
#' \cr
#' And 'broadcast' comes with some helper functions: \cr
#' \link{checkNA}, \link{checkNULL}, \cr
#' \link{bc_dim}, \link{ndim}, \link{lst.ndim}, \link{rep_dim}, \cr
#' \link{vector2array}. \cr \cr
#' 
#' 
#' @section Supported Structures:
#' 'broadcast' supports atomic/recursive arrays (up to 16 dimensions),
#' and atomic/recursive vectors. \cr
#' As in standard Linear Algebra convention,
#' dimensionless vectors are interpreted as column-vectors in broadcasted array operations. \cr
#' \cr
#' 
#' @section Method dispatch and Attribute handling:
#' The broadcasted methods
#' (i.e. all `bc_` methods, all `bc.` methods, and the operaters overloaded by the \link{broadcaster} class)
#' generally only preserve the "special" attributes between the arrays involved: \cr
#' `names`, `dimnames`, `comment` (if appropriate), and the \link{broadcaster} class attribute (and related attributes). \cr
#' Other (usually class-specific) attributes are not preserved , unlike the base 'R' operators. \cr
#' \cr
#' The reason only special attributes are preserved,
#' is because  broadcasting often results in an object with more dimensions, larger dimensions,
#' and/or larger length than the original objects. \cr
#' This is relevant as some class-specific attributes are only appropriate for certain dimensions or lengths. \cr
#' Custom matrix classes, for example, presumes an object to have exactly 2 dimensions. \cr
#' And the various classes provided by the 'bit' package have length-related attributes. \cr
#' So class attributes cannot be guaranteed to hold for the resulting objects when broadcasting is involved. \cr
#' \cr
#' Almost all functions provided by 'broadcast' are S3 or S4 generics; \cr
#' methods can be written for them for specific classes,
#' so that class-specific attributes can be supported when needed. \cr
#' 
#' @references Harris, C.R., Millman, K.J., van der Walt, S.J. et al. \emph{Array programming with NumPy}. Nature 585, 357–362 (2020). \doi{10.1038/s41586-020-2649-2}. (\href{https://www.nature.com/articles/s41586-020-2649-2}{Publisher link}).
#' 
#' @author \strong{Author, Maintainer}: Tony Wilkes \email{tonywilkes.nl@gmail.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @name aaa00_broadcast_help
#' @rdname aaa00_broadcast_help
#' @aliases broadcast-package
#' @aliases broadcast
#' @aliases broadcast_help
#' @useDynLib broadcast, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' 
NULL
#> NULL
