#' Define implied weights analysis
#'
#' \code{NitroImpliedWeights} is an S4 class that serves as the basis for
#' classes that define parameters for implied weights phylogenetic analyses.
#' @name NitroImpliedWeights-class
#' @docType class
#' @seealso The inheriting S4 classes \code{NitroImplicitEnum},
#' \code{NitroBranchswap}, \code{NitroRatchet}, \code{NitroDriven}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroImpliedWeights",
  contains = "NitroBase",
  slots = c(
    k = "numeric",
    multi_k = "logical"
  )
)

setValidity("NitroImpliedWeights", check_NitroImpliedWeights)
