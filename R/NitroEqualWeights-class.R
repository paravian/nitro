#' Define equal weights analysis
#'
#' @description
#' \code{NitroEqualWeights} is an R6 class that serves as the basis for classes
#' that define parameters for equal weights phylogenetic analyses.
#' @importFrom R6 R6Class
#' @export
NitroEqualWeights <- R6Class("NitroEqualWeights",
  inherit = NitroWeightsBase,
  public = list(
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroEqualWeights>")
    }
  )
)
