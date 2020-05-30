#' Define an implicit enumeration analysis
#'
#' \code{NitroImplicitEnum} is an S4 class that defines an implicit enumeration
#' analysis in \code{nitro}.
#' @name NitroImplicitEnum-class
#' @seealso The inheriting S4 class \code{\link{NitroMethodsBase}} and
#' \code{\link{NitroBranchSwap}}, \code{\link{NitroRatchet}} and
#' \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroImplicitEnum", contains = "NitroMethodsBase")
