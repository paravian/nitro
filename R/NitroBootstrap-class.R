#' Define bootstrap resampling analysis
#'
#' \code{NitroBootstrap} is an S4 class that defines parameters for
#' bootstrap resampling analyses.
#' @name NitroBootstrap-class
#' @docType class
#' @seealso The inheriting S4 class
#' \code{"\linkS4class{NitroConstraintsBase}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroResampleBase-class.R
setClass("NitroBootstrap",
  contains = c("NitroResampleBase"),
  slots = c(
    cutoff = "numeric"
))

setValidity("NitroBootstrap", check_NitroBootstrap)
