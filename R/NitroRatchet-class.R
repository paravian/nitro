#' Define a parsimony ratchet analysis
#'
#' \code{NitroRatchet} is an S4 class that defines the set of parameters
#' required to perform a branch swapping ('traditional', in TNTs terminology)
#' phylogenetic analysis in \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroRatchet",
  contains = "NitroMethodsBase",
  slots = c(
    iterations = "integer",
    replacements = "integer",
    prob_up = "integer",
    prob_down = "integer"
  )
)

setValidity("NitroRatchet", check_NitroRatchet)
