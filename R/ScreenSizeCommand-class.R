#' Screen Size Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the dimensions of the TNT
#' screen buffer.
#'
#' This command is created automatically as part of the standard
#' [CommandQueue] setup. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form `screen {rows}x{columns};`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `110`.
#'
#' @keywords internal
#' @importFrom checkmate asInt check_integerish test_true
#' @importFrom cli cli_abort
#' @importFrom glue glue_data
#' @importFrom R6 R6Class
ScreenSizeCommand <- R6Class(
  "ScreenSizeCommand",
  inherit = BasicCommand,
  private = list(
    .columns = NULL,
    .rows = NULL
  ),
  active = list(
    #' @field columns \[`integer(1)`\]\cr
    #'   The number of columns in the screen buffer. Must be a positive
    #'   integer.
    columns = function(value) {
      label <- "columns"
      if (missing(value)) {
        return(private$.columns)
      } else {
        val_check <- check_integerish(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an integer.",
            "x" = val_check
          ))
        }

        value <- asInt(value)
        private$.columns <- value
      }
    },
    #' @field rows \[`integer(1)`\]\cr
    #'   The number of rows in the screen buffer. Must be a positive
    #'   integer.
    rows = function(value) {
      label <- "rows"
      if (missing(value)) {
        return(private$.rows)
      } else {
        val_check <- check_integerish(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an integer.",
            "x" = val_check
          ))
        }

        value <- asInt(value)
        private$.rows <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `110`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 110)
      .queue
    },
    #' @description
    #' Create a new `ScreenSizeCommand` object.
    #'
    #' @param columns \[`integer(1)`\]\cr
    #'   The number of columns in the screen buffer. See the `$columns`
    #'   field.
    #' @param rows \[`integer(1)`\]\cr
    #'   The number of rows in the screen buffer. See the `$rows` field.
    #'
    #' @return A new `ScreenSizeCommand` object.
    initialize = function(columns, rows) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = "screen",
        description = "Dimensions of the screen buffer"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `screen` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      dim_arg <- glue_data(self, " {rows}x{columns}")
      cmd <- paste(self$name, dim_arg, ";", sep = "")
      cmd
    }
  )
)
