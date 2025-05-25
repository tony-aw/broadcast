#' Attribute handling
#' 
#' @description
#' The `bc.*` functions and the overloaded operators generally do \bold{not}
#' preserve attributes, unlike the base 'R' operators. \cr
#' \cr
#' Broadcasting often results in an object with more dimensions, larger dimensions,
#' and/or larger length than the original objects. \cr
#' Therefore, the `names`, `dimnames`, and `dim` attributes often no longer fit the new object. \cr
#' Moreover, class attributes such as `matrix` presume the object to have 2 dimensions,
#' so even class attributes cannot be guaranteed to hold for the resulting objects. \cr
#' Only some class attributes, like the 'broadcaster' class (and related) attributes,
#' will be preserved, if present.
#' 
#' 
#' @name aaa01_broadcast_attributes
#' @rdname aaa01_broadcast_attributes
#' @aliases broadcast_attributes
#' 
NULL
#> NULL
