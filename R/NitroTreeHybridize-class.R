#' Define tree hybridization properties
#'
#' \code{NitroTreeHybridize} is an S4 class that defines the set of parameters
#' required for performing tree hybridizing operations in \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroTreeHybridize",
  contains = "NitroMethodsBase",
  slots = c(
    rounds = "integer",
    hybridizations = "integer",
    best_trees = "integer",
    replace = "logical",
    sample_factor = "integer"
  )
)

setValidity("NitroTreeHybridize", check_NitroTreeHybridize)
