#' Define jacknife resampling analysis
#'
#' \code{NitroJacknife} is an S4 class that defines parameters for
#' jacknife resampling analyses.
#' @name NitroJacknife-class
#' @docType class
#' @seealso The inheriting S4 class
#' \code{"\linkS4class{NitroConstraintsBase}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroResampleBase-class.R
setClass("NitroJacknife",
  contains = c("NitroResampleBase"),
  slots = c(
    probability = "integer",
    cutoff = "numeric"
))

setValidity("NitroJacknife", check_NitroJacknife)
