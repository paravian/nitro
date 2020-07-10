#' Define tree fusing properties
#'
#' \code{NitroTreeFuse} is an S4 class that defines the set of parameters
#' required for performing tree fusing operations in \code{nitro}.
#' @name NitroTreeFuse-class
#' @seealso The S4 classes \code{"\linkS4class{NitroImplicitEnum}"},
#' \code{"\linkS4class{NitroRatchet}"} and \code{"\linkS4class{NitroDriven}"}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroTreeFuse",
  contains = "NitroMethodsBase",
  slots = c(
    rounds = "integer",
    exchange_equal = "logical",
    start_best = "logical",
    keep_all = "logical",
    accept_all = "logical",
    swap = "logical"
  )
)

setValidity("NitroTreeFuse", check_NitroTreeFuse)
