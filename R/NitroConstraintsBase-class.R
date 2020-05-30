#' Define constraints on monophyly
#'
#' \code{NitroConstraintsBase} is an S4 class that serves as the basis for classes
#' that define parameters for constraints on monophyly.
#' @name NitroTreeSearch-class
#' @docType class
#' @seealso The inheriting S4 classes \code{\link{NitroUnconstrainedSearch}}
#' and \code{\link{NitroConstrainedSearch}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroConstraintsBase",
  slots = c(
    constraints = "list"
))

setValidity("NitroConstraintsBase", check_NitroConstraintsBase)
