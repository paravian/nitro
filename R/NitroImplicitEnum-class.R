#' Define an implicit enumeration analysis
#'
#' \code{NitroImplicitEnum} is an S4 class that defines an implicit enumeration
#' analysis in \code{nitro}.
#' @name NitroImplicitEnum-class
#' @seealso The inheriting S4 class \code{"\linkS4class{NitroMethodsBase}"} and
#' \code{"\linkS4class{NitroBranchSwap}"}, \code{"\linkS4class{NitroRatchet}"} and
#' \code{"\linkS4class{NitroDriven}"}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroImplicitEnum", contains = "NitroMethodsBase")
