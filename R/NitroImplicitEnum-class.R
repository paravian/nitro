#' Define an implicit enumeration analysis
#'
#' \code{NitroImplicitEnum} is an S4 class that defines an implicit enumeration
#' analysis in \code{nitro}.
#' @name NitroImplicitEnum-class
#' @seealso The S4 classes \code{ImplicitEnum}, \code{Ratchet} and
#' \code{Driven}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroImplicitEnum", contains = "NitroTreeSearch")
