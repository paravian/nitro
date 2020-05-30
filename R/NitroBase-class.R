#' Define base parameters
#'
#' \code{NitroBase} is a virtual S4 class that defines the basic set of
#' parameters required to perform a phylogenetic analysis in \code{nitro}.
#' As this is a virtual class it cannot be constructed directly.
#' @name NitroBase-class
#' @docType class
#' @seealso The S4 classes \code{NitroTreeSearch}, \code{NitroEqualWeights}
#' and \code{NitroImpliedWeights}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroTreeSearch-class.R
setClass("NitroBase",
  contains = "VIRTUAL",
  slots = c(
    matrix = "matrix",
    tree_search = "NitroTreeSearch",
    ordered_characters = "logical",
    inactive_taxa = "logical",
    inactive_characters = "logical",
    outgroup = "integer",
    collapse = "integer"
  )
)

setValidity("NitroBase", check_NitroBase)
