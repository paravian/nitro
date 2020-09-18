#' Define symmetric resampling analysis
#'
#' \code{NitroSymmetricResample} is an S4 class that defines parameters for
#' symmetric resampling analyses.
#' @name NitroSymmetricResample-class
#' @docType class
#' @seealso The inheriting S4 class
#' \code{"\linkS4class{NitroConstraintsBase}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroResampleBase-class.R
setClass("NitroSymmetricResample",
  contains = c("NitroResampleBase"),
  slots = c(
    probability = "integer",
    cutoff = "numeric"
))

setValidity("NitroSymmetricResample", check_NitroJackknife)
