#' Define a branch swapping analysis
#'
#' \code{NitroBranchSwap} is an S4 class that defines the set of parameters
#' required to perform a branch swapping ('traditional', in TNTs terminology)
#' phylogenetic analysis in \code{nitro}.
#' @name NitroBranchSwap-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#'   \code{\link{NitroBranchSwap}}, \code{\link{NitroRatchet}} and
#'   \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroMethodsBase-class.R
setClass("NitroBranchSwap",
  contains = "NitroMethodsBase",
  slots = c(
    replications = "integer",
    hold_rep = "integer",
    keep_all = "logical"
  )
)

setValidity("NitroBranchSwap", check_NitroBranchSwap)
