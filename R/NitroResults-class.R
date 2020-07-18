setOldClass("multiPhylo")

#' Define analysis results
#'
#' \code{NitroTrees} is an S4 class that contains the
#' \code{"\linkS4class{NitroTreeSearch}"} defining the parameters for an
#' analysis the set of trees that the analysis returned.
#' @name NitroTrees-class
#' @keywords classes
#' @include check-classes.R
#' @include NitroTreeSearch-class.R
setClass("NitroTrees",
  slots = c(
    tree_search = "NitroTreeSearch",
    trees = "multiPhylo"
  )
)

setValidity("NitroTrees", check_NitroTrees)
