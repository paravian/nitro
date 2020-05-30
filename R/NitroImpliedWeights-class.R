#' Define implied weights analysis
#'
#' \code{NitroImpliedWeights} is an S4 class that serves as the basis for
#' classes that define parameters for implied weights phylogenetic analyses.
#' @name NitroImpliedWeights-class
#' @docType class
#' @seealso The inheriting S4 class \code{\link{NitroWeightsBase}} and
#' \code{\link{NitroEqualWeights}}
#' @keywords classes
#' @include check-classes.R
setClass("NitroImpliedWeights",
  contains = "NitroWeightsBase",
  slots = c(
    k = "numeric",
    weights = "numeric",
    multi_k = "logical",
    proportion = "numeric",
    max_ratio = "numeric"
  )
)

setValidity("NitroImpliedWeights", check_NitroImpliedWeights)
