#' Define tree drift properties
#'
#' \code{NitroTreeDrift} is an S4 class that defines the set of parameters
#' required for performing tree drift operations in \code{nitro}.
#' @name NitroTreeDrift-class
#' @seealso The S4 classes \code{"\linkS4class{NitroImplicitEnum}"},
#' \code{"\linkS4class{NitroRatchet}"} and \code{"\linkS4class{NitroDriven}"}.
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
