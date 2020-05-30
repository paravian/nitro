#' Define a branch swapping analysis
#'
#' \code{NitroBranchSwap} is an S4 class that defines the set of parameters
#' required to perform a branch swapping ('traditional', in TNTs terminology)
#' phylogenetic analysis in \code{nitro}.
#' @name NitroBranchSwap-class
#' @seealso The S4 classes \code{ImplicitEnum}, \code{BranchSwap}, \code{Ratchet}
#' and \code{Driven}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroBranchSwap",
  contains = "NitroTreeSearch",
  slots = c(
    replications = "integer",
    hold_rep = "integer",
    keep_all = "logical"
  )
)

setValidity("NitroBranchSwap", check_NitroBranchSwap)