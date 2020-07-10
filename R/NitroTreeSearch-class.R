#' Define tree searches
#'
#' \code{NitroTreeSearch} is an S4 class that stores information regarding the
#' tree search method, character weighting scheme and constraints on monophyly.
#' @name NitroTreeSearch-class
#' @docType class
#' @seealso The S4 classes \code{"\linkS4class{NitroConstraintsBase}"},
#' \code{"\linkS4class{NitroMethodsBase}"} and
#' \code{"\linkS4class{NitroWeightsBase}"}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroConstraintsBase-class.R
#' @include NitroMethodsBase-class.R
#' @include NitroWeightsBase-class.R
#' @include NitroResults-class.R
setClass("NitroTreeSearch",
  slots = c(
    matrix = "matrix",
    ordered_characters = "logical",
    inactive_taxa = "logical",
    inactive_characters = "logical",
    outgroup = "integer",
    collapse = "integer",
    constraints = "NitroConstraintsBase",
    method = "NitroMethodsBase",
    weights = "NitroWeightsBase",
    results = "NitroResults"
))

setValidity("NitroTreeSearch", check_NitroTreeSearch)
