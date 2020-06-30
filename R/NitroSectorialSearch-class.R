#' Define a sectorial search analysis
#'
#' \code{NitroSectorialSearch} is an S4 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroSectorialSearch",
  contains = "NitroMethodsBase",
  slots = c(
    buffer = "logical",
    slack = "integer"
  )
)

setValidity("NitroSectorialSearch", check_NitroSectorialSearch)
