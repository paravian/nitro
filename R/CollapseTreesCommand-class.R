#' Collapse Trees Command
#'
#' @description
#' An [R6][R6::R6Class] class that issues the TNT `condense` command to
#' collapse zero-length branches on trees currently held in memory.
#'
#' This command is enqueued automatically by [TreeSearchCommand] after a
#' search completes (when `$set_only` is `FALSE`). Users do not typically
#' need to instantiate it directly.
#'
#' @details
#' ## Command output
#' `$render()` produces `condense;`
#'
#' ## Queue integration
#' This command has no `$enqueue()` override; it is added to a
#' [CommandQueue] directly by [TreeSearchCommand] at priority `600`.
#'
#' @seealso
#' * [TreeSearchCommand] — enqueues this command after a search.
#' * [CollapseRuleCommand] — configures the rule used when collapsing.
#'
#' @keywords internal
#' @importFrom R6 R6Class
CollapseTreesCommand <- R6Class(
  "CollapseTreesCommand",
  inherit = BasicCommand,
  public = list(
    #' @description
    #' Create a new `CollapseTreesCommand` object.
    #'
    #' @param ... Not used.
    #'
    #' @return A new `CollapseTreesCommand` object.
    initialize = function(...) {
      super$initialize(
        name = "condense",
        description = "Collapse zero-length branches"
      )
    },
    #' @description
    #' Render the TNT `condense` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing `"condense;"`.
    render = function(...) {
      cmd <- paste(self$name, ";", sep = "")
      cmd
    }
  )
)
