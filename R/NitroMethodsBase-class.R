#' Define tree searches
#'
#' \code{NitroMethodsBase} is an S4 class that serves as the basis for classes that
#' define parameters for tree search methods.
#' @name NitroMethodsBase-class
#' @docType class
#' @seealso The inheriting S4 classes \code{"\linkS4class{NitroImplicitEnum}"},
#' \code{"\linkS4class{NitroBranchSwap}"},
#' \code{"\linkS4class{NitroRatchet}"} and
#' \code{"\linkS4class{NitroDriven}"}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroMethodsBase", contains = "VIRTUAL")
