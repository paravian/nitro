#' Define tree drift properties
#'
#' \code{NitroTreeDrift} is an S4 class that defines the set of parameters
#' required for performing tree drift operations in \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroTreeDrift",
  contains = "NitroMethodsBase",
  slots = c(
    iterations = "integer",
    substitutions = "integer",
    max_abs_fit_diff = "numeric",
    max_rel_fit_diff = "numeric",
    reject_factor = "numeric",
    autoconstrain_cycles = "integer"
  )
)

setValidity("NitroTreeDrift", check_NitroTreeDrift)
