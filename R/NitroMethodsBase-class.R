#' Define tree searches
#'
#' \code{NitroMethodsBase} is an S4 class that serves as the basis for classes that
#' define parameters for tree search methods.
#' @name NitroTreeSearch-class
#' @docType class
#' @seealso The inheriting S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroBranchSwap}}, \code{\link{NitroRatchet}} and
#' \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroMethodsBase", contains = "VIRTUAL")
