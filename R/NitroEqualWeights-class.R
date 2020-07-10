#' Define equal weights analysis
#'
#' \code{NitroEqualWeights} is an S4 class that serves as the basis for classes
#' that define parameters for equal weights phylogenetic analyses.
#' @name NitroEqualWeights-class
#' @docType class
#' @seealso The inheriting S4 class \code{"\linkS4class{NitroWeightsBase}"}
#' and \code{"\linkS4class{NitroImpliedWeights}"}.
#' @keywords classes
#' @include NitroWeightsBase-class.R
setClass("NitroEqualWeights",
  contains = "NitroWeightsBase"
)
