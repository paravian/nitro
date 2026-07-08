#' Suboptimal Search Threshold Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the suboptimal tree retention
#' thresholds used during branch support analyses in \pkg{nitro}.
#'
#' This command is created automatically by [set_support()] when branch
#' support is requested. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Thresholds
#' Two thresholds control which suboptimal trees are retained:
#' * `$absolute_threshold` — the maximum number of extra steps above the
#'   best tree length.
#' * `$relative_threshold` — the maximum proportional excess above the
#'   length of the tree being swapped, as a value between 0 and 1.
#'
#' ## Default values
#' | Parameter            | Default |
#' |----------------------|---------|
#' | `absolute_threshold` | `0`     |
#' | `relative_threshold` | `0`     |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `subopt {absolute_threshold}x{relative_threshold};`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `400`.
#'
#' @seealso
#' * [set_support()] — creates this command automatically when branch
#'   support is configured.
#'
#' @keywords internal
#' @importFrom checkmate check_number test_class test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
SuboptimalCommand <- R6Class(
  "SuboptimalCommand",
  inherit = StandardCommand,
  active = list(
    #' @field absolute_threshold \[`numeric(1)`\]\cr
    #'   The maximum number of extra steps above the best tree length for
    #'   trees to be retained. Must be a non-negative number.
    absolute_threshold = function(value) {
      label <- "absolute_threshold"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg absolute_threshold} must be a valid number.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field relative_threshold \[`numeric(1)`\]\cr
    #'   The maximum proportional excess above the length of the tree being
    #'   swapped for trees to be retained. Must be a number between 0 and
    #'   1.
    relative_threshold = function(value) {
      label <- "relative_threshold"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0, upper = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg relative_threshold} must be a valid number.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `400`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 400)
      .queue
    },
    #' @description
    #' Create a new `SuboptimalCommand` object.
    #'
    #' This command is created automatically by [set_support()]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param absolute_threshold \[`numeric(1)`\]\cr
    #'   Maximum extra steps above the best tree (default: `0`). See the
    #'   `$absolute_threshold` field.
    #' @param relative_threshold \[`numeric(1)`\]\cr
    #'   Maximum proportional excess above the swapped tree (default: `0`).
    #'   See the `$relative_threshold` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `SuboptimalCommand` object.
    initialize = function(absolute_threshold, relative_threshold, ...) {
      super$initialize(
        name = "subopt",
        description = "Suboptimal search threshold values",
        provides = "suboptimal threshold",
        ...
      )

      self$new_argument(
        label = "absolute_threshold",
        description = "Absolute suboptimal search threshold",
        command_format = "{value}",
        default_value = 0
      )
      self$new_argument(
        label = "relative_threshold",
        description = "Relative suboptimal search threshold",
        command_format = "{value}",
        default_value = 0
      )

      self$template <- "{absolute_threshold}x{relative_threshold}"

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    }
  )
)
