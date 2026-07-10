#' Echo Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures whether TNT echoes commands
#' to the console during execution.
#'
#' This command is created automatically as part of the standard
#' [CommandQueue] setup. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces `echo =;` (enabled) or `echo -;` (disabled).
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `100`, placing it near the start of the execution sequence.
#'
#' @keywords internal
#' @importFrom checkmate check_flag test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
EchoCommand <- R6Class(
  "EchoCommand",
  inherit = BasicCommand,
  private = list(
    .enable = NULL
  ),
  active = list(
    #' @field enable \[`logical(1)`\]\cr
    #'   Whether to echo commands to the console during execution.
    enable = function(value) {
      label <- "enable"
      if (missing(value)) {
        return(private$.enable)
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a logical flag.",
            "x" = val_check
          ))
        }

        private$.enable <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `100`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 100)
      .queue
    },
    #' @description
    #' Create a new `EchoCommand` object.
    #'
    #' @param enable \[`logical(1)`\]\cr
    #'   Whether to echo commands to the console. See the `$enable` field.
    #'
    #' @return A new `EchoCommand` object.
    initialize = function(enable) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = "echo",
        description = "Echo commands to console"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `echo` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      enable_arg <- ifelse(self$enable, "=", "-")
      cmd <- paste(self$name, " ", enable_arg, ";", sep = "")
      cmd
    }
  )
)
