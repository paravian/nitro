#' State Number Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the maximum number of discrete
#' character states recognised by TNT.
#'
#' This command is created automatically by [ReadDataCommand] based on the
#' matrices supplied to [TreeAnalysis]. Users do not typically need to
#' instantiate it directly.
#'
#' @details
#' ## Continuous character mode
#' When `$is_continuous` is `TRUE`, the command switches TNT into
#' continuous character mode (`nstates cont`) regardless of `$count`.
#' This is set automatically when a [ContinuousMatrix] is present in the
#' analysis.
#'
#' ## Default values
#' | Parameter       | Default |
#' |-----------------|---------|
#' | `count`         | `32`    |
#' | `is_continuous` | `FALSE` |
#'
#' ## Command output
#' `$render()` produces `nstates {count};` in discrete mode, or
#' `nstates cont;` in continuous mode.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `200`.
#'
#' @seealso
#' * [ReadDataCommand] — creates this command automatically.
#' * [ContinuousMatrix] — triggers continuous character mode.
#'
#' @keywords internal
#' @importFrom checkmate assert check_flag check_int check_null makeAssertCollection test_class test_null test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
StateNumberCommand <- R6Class(
  "StateNumberCommand",
  inherit = StandardCommand,
  active = list(
    #' @field count \[`integer(1)`\]\cr
    #'   The maximum number of states for discrete characters. Must be an
    #'   integer of at least 2. Ignored when `$is_continuous` is `TRUE`.
    count = function(value) {
      label <- "count"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 2)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a valid integer.",
                      "x" = val_check))
        }

        value <- floor(value)
        self$set_argument_value(label, value)
      }
    },
    #' @field is_continuous \[`logical(1)`\]\cr
    #'   Whether the analysis includes continuous characters. When `TRUE`,
    #'   TNT is switched into continuous character mode and `$count` is
    #'   ignored.
    is_continuous = function(value) {
      label <- "is_continuous"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a logical value.",
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
    #' Adds this command at priority `200`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 200)
      .queue
    },
    #' @description
    #' Create a new `StateNumberCommand` object.
    #'
    #' This command is created automatically by [ReadDataCommand]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param count \[`integer(1)`\]\cr
    #'   Maximum number of discrete character states (default: `32`). See
    #'   the `$count` field.
    #' @param is_continuous \[`logical(1)`\]\cr
    #'   Does the analysis include continuous characters? (default:
    #'   `FALSE`). See the `$is_continuous` field.
    #'
    #' @return A new `StateNumberCommand` object.
    initialize = function(count, is_continuous = FALSE) {
      super$initialize(
        name = "nstates",
        description = "Set maximum number of states"
      )

      self$new_argument(
        label = "count",
        description = "Maximum number of states",
        command_format = "{value}",
        default_value = 32
      )

      is_cont_cmd_fmt <- function(value) {
        ifelse(value, "cont", NA)
      }
      is_cont_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument(
        label = "is_continuous",
        description = "Use continuous characters",
        command_format = is_cont_cmd_fmt,
        default_value = FALSE,
        pretty_format = is_cont_pty_fmt
      )

      self$template <- function(...) {
        args <- list(...)
        if (test_true(args$is_continuous)) {
          return(is_cont_cmd_fmt(args$is_continuous))
        }
        args$count
      }

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
