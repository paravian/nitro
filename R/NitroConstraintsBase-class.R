#' Define constraints on monophyly
#'
#' \code{NitroConstraintsBase} is an S4 class that serves as the basis for
#' classes that define parameters for constraints on monophyly.
#' @name NitroConstraintsBase-class
#' @docType class
#' @seealso The S4 classes \code{"\linkS4class{NitroConstraint}"}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroConstraintsBase",
  slots = c(
    constraints = "list"
))

setValidity("NitroConstraintsBase", check_NitroConstraintsBase)
