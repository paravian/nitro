#' Define a random sectorial search analysis
#'
#' \code{NitroRandomSectorialSearch} is an S4 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroSectorialSearch-class.R
setClass("NitroRandomSectorialSearch",
  contains = "NitroSectorialSearch",
  slots = c(
    min_size = "integer",
    max_size = "integer",
    selection_factor = "integer",
    increase = "integer",
    small_starts = "integer"
  )
)

setValidity("NitroRandomSectorialSearch", check_NitroRandomSectorialSearch)
