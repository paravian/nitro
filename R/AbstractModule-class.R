#' Abstract module
#'
#' @description
#' An abstract module.
#'
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
AbstractModule <- R6Class("AbstractModule",
  inherit = QueueGenerator,
  private = list(
    .name = ""
  ),
  active = list(
    #' @field name The name of the module.
    name = function (value) {
      if (missing(value)) {
        return(private$.name)
      }
      cli_abort(c("{.var name} is a read-only attribute."))
    }
  )
)
