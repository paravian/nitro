#' Define an implicit enumeration analysis
#'
#' @description
#' \code{NitroImplicitEnum} is an R6 class that defines an implicit enumeration
#' analysis in \code{nitro}.
#' @importFrom R6 R6Class
#' @export
NitroImplicitEnum <- R6Class("NitroImplicitEnum",
  inherit = NitroMethodsBase,
  public = list(
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroImplicitEnum>")
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      "ienum;"
    }
  )
)
