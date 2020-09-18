#' Define jackknife resampling analysis
#'
#' \code{NitroJackknife} is an S4 class that defines parameters for
#' jackknife resampling analyses.
#' @name NitroJackknife-class
#' @docType class
#' @seealso The inheriting S4 class
#' \code{"\linkS4class{NitroConstraintsBase}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroResampleBase-class.R
setClass("NitroJackknife",
  contains = c("NitroResampleBase"),
  slots = c(
    probability = "integer",
    cutoff = "numeric"
))

setValidity("NitroJackknife", check_NitroJackknife)
