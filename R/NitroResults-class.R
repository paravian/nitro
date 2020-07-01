setOldClass("multiPhylo")

#' Define analysis results
#'
#' \code{NitroResults} is an S4 class that contains the results of an analysis
#' performed using \code{nitro}.
#' @name NitroResults-class
#' @keywords classes
#' @include check-classes.R
setClass("NitroResults",
  slots = c(
    trees = "multiPhylo"
  )
)

setValidity("NitroResults", check_NitroResults)
