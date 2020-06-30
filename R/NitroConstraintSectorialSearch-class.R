#' Define a constraint sectorial search analysis
#'
#' \code{NitroConstraintSectorialSearch} is an S4 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @name NitroRatchet-class
#' @seealso The S4 classes \code{\link{NitroImplicitEnum}},
#' \code{\link{NitroRatchet}} and \code{\link{NitroDriven}}.
#' @keywords classes
#' @include check-classes.R
#' @include NitroSectorialSearch-class.R
setClass("NitroConstraintSectorialSearch",
  contains = "NitroSectorialSearch",
  slots = c(
    min_fork = "integer",
    max_fork = "integer",
    rounds = "integer"
  )
)

setValidity("NitroConstraintSectorialSearch",
            check_NitroConstraintSectorialSearch)
