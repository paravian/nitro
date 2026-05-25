#' Unique Trees Command
#'
#' @description
#' An [R6][R6::R6Class] class that removes duplicate trees from the TNT
#' tree buffer, optionally collapsing zero-length branches first.
#'
#' This command is enqueued automatically by [ResamplingCommand] before
#' retrieving support values. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces `unique;` (standard) or `unique *;` (when
#' `$collapse` is `TRUE`, collapsing zero-length branches before
#' deduplication).
#'
#' ## Queue integration
#' This command has no `$enqueue()` override; it is added to a
#' [CommandQueue] directly by [ResamplingCommand] at priority `600`.
#'
#' @seealso
#' * [ResamplingCommand] — enqueues this command before retrieving support
#'   values.
#'
#' @keywords internal
#' @importFrom checkmate check_flag test_class test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
UniqueTreesCommand <- R6Class(
  "UniqueTreesCommand",
  inherit = BasicCommand,
  private = list(
    .collapse = NULL
  ),
  active = list(
    #' @field collapse \[`logical(1)`\]\cr
    #'   Whether to collapse zero-length branches before removing duplicate
    #'   trees.
    collapse = function(value) {
      if (missing(value)) {
        return(private$.collapse)
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a logical flag.",
                      "x" = val_check))
        }

        private$.collapse <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `UniqueTreesCommand` object.
    #'
    #' This command is created automatically by [ResamplingCommand]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param collapse \[`logical(1)`\]\cr
    #'   Collapse zero-length branches before deduplication? See the
    #'   `$collapse` field.
    #'
    #' @return A new `UniqueTreesCommand` object.
    initialize = function(collapse = TRUE) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = "unique",
        description = "Keep unique trees"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `unique` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      collapse_arg <- ifelse(private$.collapse, " *", "")
      cmd <- paste(self$name, collapse_arg, ";", sep = "")
      cmd
    }
  )
)
