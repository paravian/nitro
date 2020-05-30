#' Define a driven search
#'
#' \code{NitroBranchSwap} is an S4 class that defines the set of parameters
#' required to perform a driven search in \code{nitro}.
#' @name NitroDriven-class
#' @seealso The inheriting S4 class \code{\link{NitroMethodsBase}} and
#' \code{\link{NitroImplicitEnum}}, \code{\link{NitroBranchSwap}},
#' \code{\link{NitroRatchet}} and \code{\link{Driven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroDriven",
  contains = "NitroMethodsBase",
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
