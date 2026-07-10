#' Tree Buffer Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the size of the TNT tree
#' buffer — the maximum number of trees held in memory simultaneously.
#'
#' This command is created automatically as part of the standard
#' [CommandQueue] setup. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form `hold {size};`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `400`.
#'
#' @keywords internal
#' @importFrom checkmate asInt check_integerish test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
TreeBufferCommand <- R6Class(
  "TreeBufferCommand",
  inherit = BasicCommand,
  private = list(
    .size = NULL
  ),
  active = list(
    #' @field size \[`integer(1)`\]\cr
    #'   The maximum number of trees to hold in the TNT tree buffer. Must
    #'   be a positive integer.
    size = function(value) {
      label <- "size"
      if (missing(value)) {
        return(private$.size)
      } else {
        val_check <- check_integerish(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an integer.",
            "x" = val_check
          ))
        }

        value <- asInt(value)
        private$.size <- value
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
    #' Create a new `TreeBufferCommand` object.
    #'
    #' @param size \[`integer(1)`\]\cr
    #'   The number of trees to allocate in the tree buffer. See the
    #'   `$size` field.
    #'
    #' @return A new `TreeBufferCommand` object.
    initialize = function(size) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = "hold",
        description = "Tree buffer control",
        provides = "tree buffer"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `hold` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      cmd <- paste(self$name, " ", self$size, ";", sep = "")
      cmd
    }
  )
)
