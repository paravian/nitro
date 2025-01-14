#' Abstract tree search options
#'
#' \code{AbstractTreeSearchOptions} is an R6 class that defines properties
#'   common to tree searches. Intended to be inherited, not instantiated
#'   directly.
#' @importFrom R6 R6Class
AbstractTreeSearchOptions <- R6Class("AbstractTreeSearchOptions",
  inherit = AbstractModule,
  private = list(
    .name = "TreeSearch"
  )
)
