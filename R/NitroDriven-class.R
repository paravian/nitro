#' Define a driven search
#'
#' \code{NitroBranchSwap} is an S4 class that defines the set of parameters
#' required to perform a driven search in \code{nitro}.
#' @name NitroDriven-class
#' @seealso The inheriting S4 class \code{"\linkS4class{NitroMethodsBase}"} and
#' \code{"\linkS4class{NitroImplicitEnum}"},
#' \code{"\linkS4class{NitroBranchSwap}"},
#' \code{"\linkS4class{NitroRatchet}"} and \code{"\linkS4class{NitroDriven}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroSectorialSearch-class.R
#' @include NitroTreeFuse-class.R
#' @include NitroTreeHybridize-class.R
#' @include NitroTreeDrift-class.R
#' @include NitroRatchet-class.R
setClass("NitroDriven",
  contains = "NitroMethodsBase",
  slots = c(
    replications = "integer",
    hits = "integer",
    consense_times = "integer",
    keep_all = "logical",
    multiply = "logical",
    sectorial_search = "list",
    tree_fuse = "NitroTreeFuse",
    tree_hybridize = "NitroTreeHybridize",
    tree_drift = "NitroTreeDrift",
    ratchet = "NitroRatchet"
  )
)

setValidity("NitroDriven", check_NitroDriven)
