#' Attribute handling
#' 
#' @description
#' The `bc.` functions and the overloaded operators generally do \bold{not}
#' preserve attributes, unlike the base 'R' operators. \cr
#' \cr
#' Broadcasting often results in an object with more dimensions, larger dimensions,
#' and/or larger length than the original objects. \cr
#' Therefore, the `names`, `dimnames`, and `dim` attributes often no longer fit the new object. \cr
#' Moreover, some classes are only appropriate for certain dimensions or lengths. \cr
#' The implicit `matrix` class, for example, presumes an object to have exactly 2 dimensions. \cr
#' And the various classes provided by the 'bit' package have length-related attributes. \cr
#' So even class attributes cannot be guaranteed to hold for the resulting objects. \cr
#' \cr
#' However, the `bc.` functions and the overloaded operators
#' \bold{always} preserve the "broadcaster" attribute,
#' as this is necessary to chain together broadcasted operations. \cr
#' Notice that this contrats with base 'R' in the following sense: \cr
#' In base 'R', logical (&, |) and relational (==, !=, etc.) operators never preserve attributes,
#' whereas the broadcasted equivalents do preserve the "broadcaster" attribute. \cr
#' \cr
#' Unary operations (i.e. `+ x`, `- x`) return the original object,
#' with only the sign adjusted. \cr
#' \cr
#' 
#' 
#' @name aaa01_broadcast_attributes
#' @rdname aaa01_broadcast_attributes
#' @aliases broadcast_attributes
#' 
NULL
#> NULL
