#' Define a sectorial search analysis
#'
#' \code{NitroSectorialSearch} is an S4 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @name NitroSectorialSearch-class
#' @seealso The S4 classes \code{"\linkS4class{NitroImplicitEnum}"},
#' \code{"\linkS4class{NitroRatchet}"} and \code{"\linkS4class{NitroDriven}"}.
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
