setOldClass("phylo")

#' Define resampling parameters
#'
#' \code{NitroResampleBase} is an S4 class that defines parameters for
#' resampling analyses.
#' @name NitroResampleBase-class
#' @docType class
#' @seealso The inheriting S4 class
#' \code{"\linkS4class{NitroConstraintsBase}"}.
#' @keywords classes
#' @include NitroMethodsBase-class.R
setClass("NitroResampleBase",
  contains = c("NitroMethodsBase"),
  slots = c(
    tree_search = "NitroMethodsBase",
    phy = "phylo",
    replications = "integer",
    abs_freq_summary = "logical",
    freq_diff_summary = "logical",
    freq_slope_summary = "logical"
))

setValidity("NitroResampleBase", check_NitroResampleBase)
