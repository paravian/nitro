#' Define equal weights analysis
#'
#' \code{NitroEqualWeights} is an S4 class that serves as the basis for classes
#' that define parameters for equal weights phylogenetic analyses.
#' @name NitroEqualWeights-class
#' @docType class
#' @seealso The inheriting S4 class \code{\link{NitroWeightsBase}}
#' and \code{\link{NitroImpliedWeights}}.
#' @keywords classes
#' @include NitroWeightsBase-class.R
setClass("NitroEqualWeights",
  contains = "NitroWeightsBase"
)
