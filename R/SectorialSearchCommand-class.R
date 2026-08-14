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
            "x" = val_check
          ))
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
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `500` by default.
    #'
    #' @param .queue A [CommandQueue] object.
    #' @param priority \[`integer(1)`\]\cr
    #'   A non-negative integer controlling execution order. Lower values
    #'   execute first.
    enqueue = function(.queue, priority = 500) {
      super$enqueue(.queue, priority)
    },
    #' @description
    #' Create a new `SectorialSearchCommand` object.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   The TNT command name. See the `$name` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `SectorialSearchCommand` object.
    initialize = function(name, description, set_only = FALSE) {
      super$initialize(
        name = name,
        description = description,
        set_only = set_only
      )

      validate_topology <- function(value) {
        val_check <- check_class(value, "ReadTreesCommand")

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object"))
        }

        value
      }

      self$new_dependency("starting trees", TRUE, validate_topology)
    }
  )
)
