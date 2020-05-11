#' Define equal weights analysis
#'
#' \code{NitroEqualWeights} is an S4 class that serves as the basis for classes
#' that define parameters for equal weights phylogenetic analyses.
#' @name NitroEqualWeights-class
#' @docType class
#' @seealso The inheriting S4 classes \code{NitroImplicitEnum},
#' \code{NitroBranchswap}, \code{NitroRatchet}, \code{NitroDriven}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroEqualWeights",
  contains = "NitroBase"
)
