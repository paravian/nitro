#' Define parameters for branch breaking
#'
#' \code{NitroBranchBreak} is an S4 class that defines the set of parameters
#' required to perform branch breaking.
#' @name NitroBranchBreak-class
#' @seealso The S4 classes  \code{\link{NitroImplicitEnum}},
#'   \code{\link{NitroBranchSwap}}, \code{\link{NitroRatchet}} and
#'   \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroMethodsBase-class.R
setClass("NitroBranchBreak",
  contains = "NitroMethodsBase",
  slots = c(
    swapper = "integer",
    cluster_size = "integer",
    safe_unclip = "logical",
    fill_only = "logical",
    save_multiple = "logical",
    random_clip = "logical"
  )
)

setValidity("NitroBranchBreak", check_NitroBranchBreak)
