#' Define a driven search
#'
#' \code{NitroBranchSwap} is an S4 class that defines the set of parameters
#' required to perform a driven search in \code{nitro}.
#' @name NitroDriven-class
#' @seealso The S4 classes \code{ImplicitEnum}, \code{BranchSwap}, \code{Ratchet}
#' and \code{Driven}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroDriven",
  contains = "NitroTreeSearch",
  slots = c(
    replications = "integer",
    hits = "integer",
    rss = "logical",
    css = "logical",
    xss = "logical",
    ratchet_cycles = "integer",
    drifting_cycles = "integer",
    fusing_rounds = "integer",
    consense_times = "integer",
    keep_all = "logical"
  )
)

setValidity("NitroDriven", check_NitroDriven)
