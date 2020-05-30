#' Define tree searches
#'
#' \code{NitroWeighBase} is an S4 class that serves as the basis for classes
#' that define parameters for equal and implied weights analyses.
#' @name NitroTreeSearch-class
#' @docType class
#' @seealso The inheriting S4 classes \code{\link{NitroEqualWeights}} and
#' \code{\link{NitroImpliedWeights}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroWeightsBase", contains = "VIRTUAL")
