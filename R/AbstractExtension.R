#' Abstract extension
#'
#' @description
#' An abstract extension.
#'
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
AbstractExtension <- R6Class("AbstractExtension",
  private = list(
    .name = ""
  ),
  active = list(
    #' @field name The name of the extension.
    name = function (value) {
      if (missing(value)) {
        return(private$.name)
      }
      cli_abort(c("{.var name} is a read-only attribute."))
    }
  )
)
