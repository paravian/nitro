#' Sectorial Search Command Base Class
#'
#' @description
#' `SectorialSearchCommand` extends [TreeSearchCommand] with active bindings
#' common to all sectorial search strategies: an independent memory buffer
#' and a slack parameter for increasing available memory during searches.
#'
#' This class is not exported. It is intended to be subclassed by
#' [ConstrainedSectorialSearchCommand], [ExclusiveSectorialSearchCommand],
#' and [RandomSectorialSearchCommand].
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description)` and then
#' register their own arguments with `$new_argument()`. The `$buffer` and
#' `$slack` active bindings are defined here, but subclasses are
#' responsible for registering the corresponding `buffer` and `slack`
#' arguments via `$new_argument()` with appropriate command formats, so
#' that the argument labels match these bindings.
#'
#' ## Shared fields
#' | Field | Type | Description |
#' |-------|------|-------------|
#' | `buffer` | `logical(1)` | Use an independent memory buffer for sector analysis. |
#' | `slack` | `integer(1)` | Percentage to increase available memory during searches. |
#'
#' @seealso
#' * [TreeSearchCommand] — parent class providing set-only mode and queue
#'   integration.
#' * [ConstrainedSectorialSearchCommand] — constraint-based sectorial
#'   searches.
#' * [ExclusiveSectorialSearchCommand] — exclusive (non-overlapping)
#'   sectorial searches.
#' * [RandomSectorialSearchCommand] — random sectorial searches.
#'
#' @keywords internal
#' @importFrom checkmate check_int check_flag test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
SectorialSearchCommand <- R6Class(
  "SectorialSearchCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field buffer \[`logical(1)`\]\cr
    #'   Whether to use an independent memory buffer for analysis of sectors.
    buffer = function(value) {
      label <- "buffer"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg buffer} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field slack \[`integer(1)`\]\cr
    #'   The percentage by which to increase available memory during
    #'   searches. Must be a non-negative integer.
    slack = function(value) {
      label <- "slack"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg slack} must be an integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  )
)
